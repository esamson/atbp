package ph.samson.atbp.http

import zio.Ref
import zio.Scope
import zio.ZIO
import zio.durationInt
import zio.http.*
import zio.stream.ZStream
import zio.test.*

import java.io.IOException

object StatusCheckSpec extends ZIOSpecDefault {

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
      err: Throwable
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
          case true  => ZIO.fail(err)
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
        _ <- TestClock.adjust(1.hour)
        body <- fiber.join
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
        _ <- TestClock.adjust(1.hour)
        res <- fiber.join
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
          new IOException("connection reset")
        )
        fiber <- checked(failing, StatusCheck.SafeMethods)
          .batched(Request.put(URL.root / "item", Body.fromString("{}")))
          .fork
        _ <- TestClock.adjust(1.hour)
        res <- fiber.join
        n <- hits.get
      } yield assertTrue(res.status.isSuccess, n == 1)
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
        _ <- TestClock.adjust(1.hour)
        body <- fiber.join
        n <- hits.get
      } yield assertTrue(body == "hits", n == 2)
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
        _ <- TestClock.adjust(1.hour)
        result <- fiber.join
        n <- hits.get
      } yield assertTrue(n == 11, result.isLeft)
    }
  ).provideSomeLayer(TestClient.layer)
}
