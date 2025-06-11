package ph.samson.atbp.retext

import better.files.File
import org.apache.commons.io.ByteOrderMark
import org.apache.commons.io.input.BOMInputStream
import zio.Task
import zio.ZIO

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintWriter
import java.nio.charset.Charset

object Reencoder {

  def transform(
      source: File,
      sourceCharset: Charset,
      target: File,
      targetCharset: Charset
  ): Task[Unit] = {
    val reader = sourceCharset.name() match {
      case "UTF-8" | "UTF-16" | "UTF-16BE" | "UTF-16LE" | "UTF-32BE" |
          "UTF-32LE" =>
        ZIO.attemptBlockingIO(
          new BufferedReader(
            new InputStreamReader(
              BOMInputStream
                .builder()
                .setInputStream(source.newInputStream)
                .setByteOrderMarks(
                  ByteOrderMark.UTF_8,
                  ByteOrderMark.UTF_16BE,
                  ByteOrderMark.UTF_16LE,
                  ByteOrderMark.UTF_32BE,
                  ByteOrderMark.UTF_32LE
                )
                .get(),
              sourceCharset
            )
          )
        )
      case _ =>
        ZIO.attemptBlockingIO(source.newBufferedReader(using sourceCharset))
    }
    val writer = ZIO.attempt(
      new PrintWriter(target.newBufferedWriter(using targetCharset))
    )

    ZIO.scoped {
      for {
        r <- ZIO.acquireRelease(reader)(reader =>
          ZIO.succeedBlocking(reader.close())
        )
        w <- ZIO.acquireRelease(writer)(writer =>
          ZIO.succeedBlocking(writer.close())
        )
        _ <- ZIO.attemptBlockingIO(
          r.lines().forEachOrdered(line => w.println(line))
        )
      } yield ()
    }
  }
}
