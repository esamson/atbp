package ph.samson.atbp.traceviz

import better.files.File
import com.github.tototoshi.csv.CSVReader
import zio.Console
import zio.ZIO

import java.time.LocalDateTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.TimeUnit
import scala.util.control.NoStackTrace

/** Writes out Trace Event Format JSON files
  *
  * @see
  *   https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU
  */
object TefJson {
  private val Fmt =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  case class Entry(name: String, start: ZonedDateTime, end: ZonedDateTime)

  def convert(source: File) = ZIO.logSpan("convert") {
    for {
      rows <- ZIO.attemptBlockingIO {
        CSVReader.open(source.toJava).all()
      }
      entries <- ZIO.foreachPar(rows) {
        case name :: start :: end :: Nil =>
          ZIO.attempt(
            Entry(
              name,
              LocalDateTime.parse(start, Fmt).atZone(ZoneId.systemDefault()),
              LocalDateTime.parse(end, Fmt).atZone(ZoneId.systemDefault())
            )
          )
        case other => ZIO.fail(BadRow(other))
      }
      tef = entries
        .sortWith((left, right) =>
          if (left.start != right.start) {
            left.start.isBefore(right.start)
          } else {
            right.end.isBefore(left.end)
          }
        )
        .flatMap { entry =>
          val startMicros = TimeUnit.SECONDS.toMicros(entry.start.toEpochSecond)
          val endMicros = TimeUnit.SECONDS.toMicros(entry.end.toEpochSecond)
          List(
            s"""{
               |    "name": "${entry.name}",
               |    "cat": "PERF",
               |    "ph": "B",
               |    "pid": "${source.nameWithoutExtension}",
               |    "tid": "${entry.name}",
               |    "ts": $startMicros
               |}""".stripMargin,
            s"""{
               |    "name": "${entry.name}",
               |    "cat": "PERF",
               |    "ph": "E",
               |    "pid": "${source.nameWithoutExtension}",
               |    "tid": "${entry.name}",
               |    "ts": $endMicros
               |}""".stripMargin
          )
        }
      _ <- Console.printLine(tef.mkString("[", ",\n", "]"))
    } yield ()
  }

  case class BadRow(row: List[String]) extends Exception with NoStackTrace
}
