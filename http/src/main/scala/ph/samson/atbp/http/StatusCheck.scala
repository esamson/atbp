package ph.samson.atbp.http

import zio.Clock
import zio.Duration
import zio.Schedule
import zio.Scope
import zio.Trace
import zio.ZIO
import zio.durationInt
import zio.http.*
import zio.http.Header.RetryAfter
import zio.http.Status.ServerError
import zio.http.Status.TooManyRequests

object StatusCheck {

  def successOnly(): ZClientAspect[
    Nothing,
    Any,
    Nothing,
    Body,
    Throwable,
    Any,
    Nothing,
    Response
  ] = statusCheck(accept = _.isSuccess)

  def statusCheck(accept: Status => Boolean): ZClientAspect[
    Nothing,
    Any,
    Nothing,
    Body,
    Throwable,
    Any,
    Nothing,
    Response
  ] = new ZClientAspect[
    Nothing,
    Any,
    Nothing,
    Body,
    Throwable,
    Any,
    Nothing,
    Response
  ] {

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
        )(implicit trace: Trace): ZIO[Env & ReqEnv, Err, Response] = {
          val doRequest = for {
            response <- oldDriver.request(
              version,
              method,
              url,
              headers,
              body,
              sslConfig,
              proxy
            )
            status = response.status
            _ <- ZIO.when(!accept(status)) {
              for {
                body <- response.body.asString
                headers = response.headers
                f <- ZIO.fail(BadStatus(method, url, status, headers, body))
              } yield f
            }
          } yield response
          doRequest.retry {
            val policy =
              Schedule.exponential(1.second).jittered && Schedule.recurs(10) &&
                Schedule.recurWhileZIO {
                  case BadStatus(_, _, status, headers, _) =>
                    status match {
                      case _: ServerError | TooManyRequests =>
                        // 429 and 5xx should be retried
                        headers.get(RetryAfter) match {
                          case Some(retryAfter) =>
                            // if "Retry-After" is specified, follow that
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
                                } yield {
                                  true
                                }
                              case RetryAfter.ByDuration(duration) =>
                                ZIO.logWarning(
                                  s"slowing down for $status; delaying for $duration"
                                ) *> ZIO.succeed(true).delay(duration)
                            }
                          case None =>
                            // no "Retry-After", just use exponential delay
                            ZIO.succeed(true)
                        }
                      case _ => ZIO.succeed(false)
                    }
                  case _ => ZIO.succeed(false)
                }
            policy.tapOutput(o =>
              ZIO.logWarning(s"retrying $method $url after $o")
            )
          }
        }

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
