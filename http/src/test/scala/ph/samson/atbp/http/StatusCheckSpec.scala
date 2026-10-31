package ph.samson.atbp.http

import io.netty.handler.timeout.ReadTimeoutException
import io.netty.handler.timeout.WriteTimeoutException
import zio.Fiber
import zio.Promise
import zio.Ref
import zio.Scope
import zio.ZIO
import zio.durationInt
import zio.http.*
import zio.stream.ZStream
import zio.test.*

import java.io.IOException

object StatusCheckSpec extends ZIOSpecDefault {

  // A driver or streamed body can suspend outside TestClock. Keep advancing
  // until the request finishes so a retry registered after an advance still runs.
  private def awaitWithClock[E, A](fiber: Fiber[E, A]): ZIO[Any, E, A] =
    TestClock
      .adjust(1.hour)
      .repeatUntilZIO(_ => fiber.poll.map(_.isDefined)) *> fiber.join

  private def truncated =
    Body.fromStreamChunked(ZStream.fail(new IOException("connection closed")))

  private def checked(
      client: Client,
      replayAfterSuccess: Method => Boolean
  ) =
    client @@ StatusCheck.successOnly(replayAfterSuccess)

  private def failFirstRequest(
      client: Client,
      first: Ref[Boolean],
      failure: ZIO[Any, Throwable, Nothing]
  ): Client = {
    val oldDriver = client.driver
    val newDriver = new ZClient.Driver[Any, Scope, Throwable] {
      override def request(
          version: Version,
          method: Method,
          url: URL,
          headers: Headers,
          body: Body,
          sslConfig: Option[ClientSSLConfig],
          proxy: Option[Proxy]
      )(implicit trace: zio.Trace): ZIO[Any & Scope, Throwable, Response] =
        first.getAndSet(false).flatMap {
          case true  => failure
          case false =>
            oldDriver.request(
              version,
              method,
              url,
              headers,
              body,
              sslConfig,
              proxy
            )
        }

      override def socket[Env1 <: Any](
          version: Version,
          url: URL,
          headers: Headers,
          app: WebSocketApp[Env1]
      )(implicit
          trace: zio.Trace,
          ev: Scope =:= Scope
      ): ZIO[Env1 & Scope, Throwable, Response] =
        oldDriver.socket(version, url, headers, app)
    }
    client.transform(client.bodyEncoder, client.bodyDecoder, newDriver)
  }

  override def spec = suite("StatusCheck")(
    test("SafeMethods are GET/HEAD/OPTIONS; SafePlusPost also allows POST") {
      assertTrue(
        StatusCheck.SafeMethods(Method.GET),
        StatusCheck.SafeMethods(Method.HEAD),
        StatusCheck.SafeMethods(Method.OPTIONS),
        !StatusCheck.SafeMethods(Method.POST),
        !StatusCheck.SafeMethods(Method.PUT),
        !StatusCheck.SafeMethods(Method.DELETE),
        !StatusCheck.SafeMethods(Method.PATCH),
        StatusCheck.SafePlusPost(Method.GET),
        StatusCheck.SafePlusPost(Method.POST),
        !StatusCheck.SafePlusPost(Method.PUT)
      )
    },
    test("retries GET when a successful response body is cut off") {
      for {
        hits <- Ref.make(0)
        _ <- TestClient.addRoute(
          Method.GET / "item" -> handler { (_: Request) =>
            hits.updateAndGet(_ + 1).map {
              case 1 => Response.ok.copy(body = truncated)
              case _ => Response.text("recovered")
            }
          }
        )
        client <- ZIO.service[Client]
        fiber <- checked(client, StatusCheck.SafeMethods)
          .batched(Request.get(URL.root / "item"))
          .flatMap(_.body.asString)
          .fork
        body <- awaitWithClock(fiber)
        n <- hits.get
      } yield assertTrue(body == "recovered", n == 2)
    },
    test("does not replay PUT after a successful status") {
      for {
        hits <- Ref.make(0)
        _ <- TestClient.addRoute(
          Method.PUT / "item" -> handler { (_: Request) =>
            hits.updateAndGet(_ + 1).as(Response.ok.copy(body = truncated))
          }
        )
        client <- ZIO.service[Client]
        result <- checked(client, StatusCheck.SafeMethods)
          .batched(Request.put(URL.root / "item", Body.fromString("{}")))
          .flatMap(_.body.asString)
          .either
        n <- hits.get
      } yield result match {
        case Left(err: IOException) =>
          assertTrue(n == 1, err.getMessage.contains("connection closed"))
        case other =>
          assertNever(s"expected IOException, got $other")
      }
    },
    test("does not replay POST after a successful status by default") {
      for {
        hits <- Ref.make(0)
        _ <- TestClient.addRoute(
          Method.POST / "search" -> handler { (_: Request) =>
            hits.updateAndGet(_ + 1).as(Response.ok.copy(body = truncated))
          }
        )
        client <- ZIO.service[Client]
        result <- (client @@ StatusCheck.successOnly())
          .batched(Request.post(URL.root / "search", Body.fromString("{}")))
          .flatMap(_.body.asString)
          .either
        n <- hits.get
      } yield result match {
        case Left(err: IOException) =>
          assertTrue(n == 1, err.getMessage.contains("connection closed"))
        case other =>
          assertNever(s"expected IOException, got $other")
      }
    },
    test("retries PUT when a server error body is cut off") {
      for {
        hits <- Ref.make(0)
        _ <- TestClient.addRoute(
          Method.PUT / "item" -> handler { (_: Request) =>
            hits.updateAndGet(_ + 1).map {
              case 1 =>
                Response
                  .status(Status.InternalServerError)
                  .copy(body = truncated)
              case _ => Response.ok
            }
          }
        )
        client <- ZIO.service[Client]
        fiber <- checked(client, StatusCheck.SafeMethods)
          .batched(Request.put(URL.root / "item", Body.fromString("{}")))
          .fork
        res <- awaitWithClock(fiber)
        n <- hits.get
      } yield assertTrue(res.status.isSuccess, n == 2)
    },
    test("retries PUT when the connection fails before a status") {
      for {
        hits <- Ref.make(0)
        first <- Ref.make(true)
        _ <- TestClient.addRoute(
          Method.PUT / "item" -> handler { (_: Request) =>
            hits.update(_ + 1).as(Response.ok)
          }
        )
        client <- ZIO.service[Client]
        failing = failFirstRequest(
          client,
          first,
          // Let the first clock advance finish before registering the retry.
          Live.live(ZIO.sleep(100.millis)) *>
            ZIO.fail(new IOException("connection reset"))
        )
        fiber <- checked(failing, StatusCheck.SafeMethods)
          .batched(Request.put(URL.root / "item", Body.fromString("{}")))
          .fork
        res <- awaitWithClock(fiber)
        n <- hits.get
      } yield assertTrue(res.status.isSuccess, n == 1)
    },
    test("retries a Netty read timeout before receiving a status") {
      for {
        first <- Ref.make(true)
        _ <- TestClient.addRoute(
          Method.GET / "item" -> handler(Response.ok)
        )
        client <- ZIO.service[Client]
        failing = failFirstRequest(
          client,
          first,
          ZIO.fail(ReadTimeoutException.INSTANCE)
        )
        fiber <- checked(failing, StatusCheck.SafeMethods)
          .batched(Request.get(URL.root / "item"))
          .either
          .fork
        result <- awaitWithClock(fiber)
      } yield assertTrue(result.exists(_.status.isSuccess))
    },
    test("retries a Netty write timeout before receiving a status") {
      for {
        first <- Ref.make(true)
        _ <- TestClient.addRoute(
          Method.GET / "item" -> handler(Response.ok)
        )
        client <- ZIO.service[Client]
        failing = failFirstRequest(
          client,
          first,
          ZIO.fail(WriteTimeoutException.INSTANCE)
        )
        fiber <- checked(failing, StatusCheck.SafeMethods)
          .batched(Request.get(URL.root / "item"))
          .exit
          .fork
        result <- awaitWithClock(fiber)
      } yield assertTrue(result.exists(_.status.isSuccess))
    },
    test("retries GET when the response body reports a read timeout") {
      for {
        hits <- Ref.make(0)
        _ <- TestClient.addRoute(
          Method.GET / "item" -> handler { (_: Request) =>
            hits.updateAndGet(_ + 1).map {
              case 1 =>
                Response.ok.copy(
                  body = Body.fromStreamChunked(
                    ZStream.fail(ReadTimeoutException.INSTANCE)
                  )
                )
              case _ => Response.text("recovered")
            }
          }
        )
        client <- ZIO.service[Client]
        fiber <- checked(client, StatusCheck.SafeMethods)
          .batched(Request.get(URL.root / "item"))
          .flatMap(_.body.asString)
          .exit
          .fork
        result <- awaitWithClock(fiber)
        n <- hits.get
      } yield assertTrue(result.exists(_ == "recovered"), n == 2)
    },
    test("does not treat an InterruptedException as a transport failure") {
      for {
        first <- Ref.make(true)
        _ <- TestClient.addRoute(Method.GET / "item" -> handler(Response.ok))
        client <- ZIO.service[Client]
        err = new InterruptedException("request interrupted")
        failing = failFirstRequest(client, first, ZIO.fail(err))
        fiber <- checked(failing, StatusCheck.SafeMethods)
          .batched(Request.get(URL.root / "item"))
          .either
          .fork
        result <- awaitWithClock(fiber)
      } yield assertTrue(result == Left(err))
    },
    test("preserves an interruption reported by the HTTP driver") {
      for {
        first <- Ref.make(true)
        _ <- TestClient.addRoute(Method.GET / "item" -> handler(Response.ok))
        client <- ZIO.service[Client]
        failing = failFirstRequest(client, first, ZIO.interrupt)
        fiber <- checked(failing, StatusCheck.SafeMethods)
          .batched(Request.get(URL.root / "item"))
          .exit
          .fork
        result <- awaitWithClock(fiber)
      } yield assertTrue(result.isInterrupted)
    },
    test("does not replay PUT after its successful body is interrupted") {
      for {
        hits <- Ref.make(0)
        _ <- TestClient.addRoute(
          Method.PUT / "item" -> handler { (_: Request) =>
            hits
              .update(_ + 1)
              .as(
                Response.ok.copy(
                  body = Body.fromStreamChunked(ZStream.fromZIO(ZIO.interrupt))
                )
              )
          }
        )
        client <- ZIO.service[Client]
        result <- checked(client, StatusCheck.SafeMethods)
          .batched(Request.put(URL.root / "item", Body.fromString("{}")))
          .exit
        n <- hits.get
      } yield assertTrue(result.isFailure, n == 1)
    },
    test(
      "external cancellation stops an in-flight request without replaying it"
    ) {
      for {
        hits <- Ref.make(0)
        first <- Ref.make(true)
        started <- Promise.make[Nothing, Unit]
        _ <- TestClient.addRoute(
          Method.GET / "item" -> handler { (_: Request) =>
            hits.update(_ + 1).as(Response.ok)
          }
        )
        client <- ZIO.service[Client]
        failing = failFirstRequest(
          client,
          first,
          started.succeed(()) *> ZIO.never
        )
        fiber <- checked(failing, StatusCheck.SafeMethods)
          .batched(Request.get(URL.root / "item"))
          .fork
        _ <- started.await
        result <- fiber.interrupt
        n <- hits.get
      } yield assertTrue(result.isInterrupted, n == 0)
    },
    test("replays POST when the caller opts in") {
      for {
        hits <- Ref.make(0)
        _ <- TestClient.addRoute(
          Method.POST / "search" -> handler { (_: Request) =>
            hits.updateAndGet(_ + 1).map {
              case 1 => Response.ok.copy(body = truncated)
              case _ => Response.text("hits")
            }
          }
        )
        client <- ZIO.service[Client]
        fiber <- checked(client, StatusCheck.SafePlusPost)
          .batched(Request.post(URL.root / "search", Body.fromString("{}")))
          .flatMap(_.body.asString)
          .fork
        body <- awaitWithClock(fiber)
        n <- hits.get
      } yield assertTrue(body == "hits", n == 2)
    },
    test("caps response timeout retries at ten recurrences") {
      for {
        hits <- Ref.make(0)
        _ <- TestClient.addRoute(
          Method.GET / "item" -> handler { (_: Request) =>
            hits
              .update(_ + 1)
              .as(
                Response.ok.copy(
                  body = Body.fromStreamChunked(
                    ZStream.fail(ReadTimeoutException.INSTANCE)
                  )
                )
              )
          }
        )
        client <- ZIO.service[Client]
        fiber <- checked(client, StatusCheck.SafeMethods)
          .batched(Request.get(URL.root / "item"))
          .exit
          .fork
        result <- awaitWithClock(fiber)
        n <- hits.get
      } yield assertTrue(n == 11, result.isFailure)
    },
    test("caps connection retries at ten recurrences") {
      for {
        hits <- Ref.make(0)
        _ <- TestClient.addRoute(
          Method.GET / "item" -> handler { (_: Request) =>
            hits.update(_ + 1).as(Response.ok.copy(body = truncated))
          }
        )
        client <- ZIO.service[Client]
        fiber <- checked(client, StatusCheck.SafeMethods)
          .batched(Request.get(URL.root / "item"))
          .flatMap(_.body.asString)
          .either
          .fork
        result <- awaitWithClock(fiber)
        n <- hits.get
      } yield assertTrue(n == 11, result.isLeft)
    }
  ).provideSomeLayer(TestClient.layer) @@ TestAspect.timeout(20.seconds)
}
