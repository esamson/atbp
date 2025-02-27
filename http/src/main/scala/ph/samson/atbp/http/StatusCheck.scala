package ph.samson.atbp.http

import zio.Schedule
import zio.Scope
import zio.Trace
import zio.ZIO
import zio.durationInt
import zio.http.*

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
  ] = statusCheck(accept = _.isSuccess, shouldRetry = _.isServerError)

  def statusCheck(
      accept: Status => Boolean,
      shouldRetry: Status => Boolean
  ): ZClientAspect[
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
                f <- ZIO.fail(BadStatus(method, url, status, body))
              } yield f
            }
          } yield response
          doRequest.retry {
            val policy =
              Schedule.exponential(1.second).jittered && Schedule.recurs(10) &&
                Schedule.recurWhile {
                  case BadStatus(_, _, status, _) => shouldRetry(status)
                  case _                          => false
                }
            policy.tapOutput(o =>
              ZIO.logWarning(s"retrying ($o): $method $url")
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
      body: String
  ) extends Exception(
        s"${method.name} ${url.path} returned ${status.reasonPhrase}: $body"
      )
}
