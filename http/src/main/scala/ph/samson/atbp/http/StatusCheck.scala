package ph.samson.atbp.http

import zio.Clock
import zio.Duration
import zio.Schedule
import zio.Scope
import zio.Trace
import zio.UIO
import zio.ZIO
import zio.http.*
import zio.http.Header.RetryAfter
import zio.http.Status.ServerError
import zio.http.Status.TooManyRequests

object StatusCheck {

  private type Aspect =
    ZClientAspect[
      Nothing,
      Any,
      Nothing,
      Body,
      Throwable,
      Any,
      Nothing,
      Response
    ]

  /** True when re-issuing the request is safe after a successful status was
    * already observed (for example the response body was cut off mid-stream).
    * POST/PUT/DELETE may already have been applied; replaying them can create
    * duplicates or hit optimistic-lock conflicts.
    */
  val SafeMethods: Method => Boolean =
    method =>
      method == Method.GET || method == Method.HEAD || method == Method.OPTIONS

  /** [[SafeMethods]] plus POST. Use when every POST on the client is a read
    * (for example Jira JQL search).
    */
  val SafePlusPost: Method => Boolean =
    method => SafeMethods(method) || method == Method.POST

  def successOnly(): Aspect = successOnly(SafeMethods)

  def successOnly(replayAfterSuccess: Method => Boolean): Aspect =
    statusCheck(_.isSuccess, replayAfterSuccess)

  def statusCheck(accept: Status => Boolean): Aspect =
    statusCheck(accept, SafeMethods)

  def statusCheck(
      accept: Status => Boolean,
      replayAfterSuccess: Method => Boolean
  ): Aspect = new Aspect {

    override def apply[
        ReqEnv,
        Env >: Nothing <: Any,
        In >: Nothing <: Body,
        Err >: Throwable <: Any,
        Out >: Nothing <: Response
    ](
        client: ZClient[Env, ReqEnv, In, Err, Out]
    ): ZClient[Env, ReqEnv, In, Err, Out] = {
      val oldDriver = client.driver

      val newDriver = new ZClient.Driver[Env, ReqEnv, Err] {
        override def request(
            version: Version,
            method: Method,
            url: URL,
            headers: Headers,
            body: Body,
            sslConfig: Option[ClientSSLConfig],
            proxy: Option[Proxy]
        )(implicit trace: Trace): ZIO[Env & ReqEnv, Err, Response] =
          oldDriver
            .request(
              version,
              method,
              url,
              headers,
              body,
              sslConfig,
              proxy
            )
            .flatMap(
              bufferAndCheck(_, accept, replayAfterSuccess, method, url)
            )
            .retry {
              val policy =
                ConnectionRetry.budget &&
                  Schedule.recurWhileZIO {
                    case BadStatus(_, _, status, headers, _) =>
                      retryServerError(status, headers)
                    case other: Throwable =>
                      ConnectionRetry.isRetryable(other)
                    case _ => ZIO.succeed(false)
                  }
              policy.tapOutput(o =>
                ZIO.logWarning(s"retrying $method $url after $o")
              )
            }
            .absolve

        override def socket[Env1 <: Env](
            version: Version,
            url: URL,
            headers: Headers,
            app: WebSocketApp[Env1]
        )(implicit
            trace: Trace,
            ev: ReqEnv =:= Scope
        ): ZIO[Env1 & ReqEnv, Err, Response] =
          oldDriver.socket(version, url, headers, app)
      }

      client.transform(client.bodyEncoder, client.bodyDecoder, newDriver)
    }
  }

  private def bufferAndCheck(
      response: Response,
      accept: Status => Boolean,
      replayAfterSuccess: Method => Boolean,
      method: Method,
      url: URL
  ): ZIO[Any, Throwable, Either[Throwable, Response]] = {
    val status = response.status
    response.body.asChunk.foldZIO(
      err =>
        if (accept(status) && !replayAfterSuccess(method)) {
          ZIO.succeed(Left(err))
        } else {
          ZIO.fail(err)
        },
      bytes => {
        val buffered = response.copy(body = Body.fromChunk(bytes))
        if (accept(status)) {
          ZIO.succeed(Right(buffered))
        } else {
          buffered.body.asString.flatMap { body =>
            ZIO.fail(BadStatus(method, url, status, buffered.headers, body))
          }
        }
      }
    )
  }

  private def retryServerError(
      status: Status,
      headers: Headers
  ): UIO[Boolean] =
    status match {
      case _: ServerError | TooManyRequests =>
        headers.get(RetryAfter) match {
          case Some(retryAfter) =>
            retryAfter match {
              case RetryAfter.ByDate(date) =>
                for {
                  now <- Clock.currentDateTime
                  duration = Duration.fromInterval(
                    now,
                    date.toOffsetDateTime
                  )
                  _ <- ZIO.logWarning(
                    s"slowing down for $status; waiting until $date"
                  )
                  _ <- ZIO.sleep(duration)
                } yield true
              case RetryAfter.ByDuration(duration) =>
                ZIO.logWarning(
                  s"slowing down for $status; delaying for $duration"
                ) *> ZIO.succeed(true).delay(duration)
            }
          case None => ZIO.succeed(true)
        }
      case other =>
        ZIO.logWarning(s"not retrying other status: $other") *>
          ZIO.succeed(false)
    }

  case class BadStatus(
      method: Method,
      url: URL,
      status: Status,
      headers: Headers,
      body: String
  ) extends Exception(
        s"${method.name} ${url.path} returned ${status.reasonPhrase}: $body [$headers]"
      )
}
