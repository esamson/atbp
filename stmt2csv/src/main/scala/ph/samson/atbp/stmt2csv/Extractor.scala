package ph.samson.atbp.stmt2csv

import better.files.File
import com.github.tototoshi.csv.CSVWriter
import ph.samson.atbp.stmt2csv.parsers.BdoCreditCardParser
import ph.samson.atbp.stmt2csv.parsers.BpiAccountParser
import ph.samson.atbp.stmt2csv.parsers.BpiCreditCardParser
import ph.samson.atbp.stmt2csv.parsers.MayaSavingsParser
import ph.samson.atbp.stmt2csv.parsers.StatementParser
import ph.samson.atbp.stmt2csv.parsers.UnionBankCreditCardParser
import zio.Task
import zio.ZIO

object Extractor {

  val parsers: List[StatementParser] =
    List(
      BdoCreditCardParser,
      BpiAccountParser,
      BpiCreditCardParser,
      MayaSavingsParser,
      UnionBankCreditCardParser
    )

  def extract(source: File, target: File): Task[File] = for {
    _ <- ZIO.logInfo(s"loading $source (${source.size()} bytes)")
    parsed <- parsers match {
      case head :: next =>
        ZIO.raceAll(head.apply(source), next.map(_.apply(source)))
      case Nil => ZIO.fail(new IllegalStateException("No parsers"))
    }
    _ <- ZIO.logDebug(s"result:\n${parsed.mkString("\n")}")
    - <- ZIO.acquireReleaseWith(
      ZIO.attemptBlockingIO(CSVWriter.open(target.toJava))
    )(w => ZIO.succeedBlocking(w.close())) { writer =>
      ZIO.attemptBlockingIO(writer.writeAll(parsed.map {
        case CsvEntry(date, description, amount) =>
          List(date, description, amount)
      }))
    }
  } yield {
    target
  }

  def extractText(source: File, target: File): Task[File] = for {
    _ <- ZIO.logInfo(s"loading $source (${source.size()} bytes)")
    extract <- parsers match {
      case head :: next =>
        ZIO.raceAll(head.extractText(source), next.map(_.extractText(source)))
      case Nil => ZIO.fail(new IllegalStateException("No parsers"))
    }
    _ <- ZIO.logDebug(s"result:\n$extract")
    - <- ZIO.attemptBlockingIO(target.write(extract))
  } yield {
    target
  }
}
