package ph.samson.atbp.http

import io.netty.channel.unix.Errors.NativeIoException
import io.netty.handler.codec.PrematureChannelClosureException
import zio.Schedule
import zio.ZIO
import zio.durationInt

import java.io.IOException

/** Retry policy for transient connection failures, e.g. a server closing the
  * connection before the response body finishes streaming. Used by
  * [[StatusCheck]], which materializes the response body so a mid-stream close
  * is retried with the same budget as a header-phase failure.
  */
object ConnectionRetry {

  def isRetryable(t: Throwable): ZIO[Any, Nothing, Boolean] = t match {
    case _: PrematureChannelClosureException => ZIO.succeed(true)
    case nie: NativeIoException              =>
      ZIO.logWarning(s"retrying NativeIoException: $nie") *> ZIO.succeed(true)
    case ioe: IOException =>
      ZIO.logWarning(s"retrying IOException: $ioe") *> ZIO.succeed(true)
    case other =>
      ZIO.logWarning(s"not retrying other exception: $other") *> ZIO.succeed(
        false
      )
  }

  val budget =
    Schedule.exponential(1.second).jittered && Schedule.recurs(10)
}
