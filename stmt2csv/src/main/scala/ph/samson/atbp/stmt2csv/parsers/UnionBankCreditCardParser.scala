package ph.samson.atbp.stmt2csv.parsers

import fastparse.*
import org.apache.pdfbox.text.PDFTextStripper
import ph.samson.atbp.stmt2csv.CsvEntry
import zio.Task
import zio.ZIO

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import NoWhitespace.*

object UnionBankCreditCardParser extends StatementParser {

  override def stripper: PDFTextStripper = {
    val s = new PDFTextStripper
    s
  }

  override def validate(text: String): Task[String] =
    if (text.contains("UnionBank Credit Card")) {
      ZIO.succeed(text)
    } else {
      ZIO.fail(
        new IllegalArgumentException("Not a UnionBank Credit Card Statement")
      )
    }

  override def parseEntries(text: String): Task[List[CsvEntry]] = ZIO.attempt {
    val transactions = parse(text, statement(using _)).get.value
    transactions.toList.map {
      case Transaction(transactionDate, _, description, amount) =>
        CsvEntry(transactionDate, description, amount)
    }
  }

  sealed abstract class Line
  case class Boring(value: String) extends Line
  case class Transaction(
      transactionDate: LocalDate,
      postDate: LocalDate,
      description: String,
      amount: BigDecimal
  ) extends Line

  def statement[T: P] = P(
    (!transaction ~ anyLine).rep ~
      (transaction | anyLine.map(Boring.apply)).rep
  ).map { case (_, lines) =>
    lines.collect { case t: Transaction => t }
  }

  def transaction[T: P] = P(
    date ~ " " ~
      date ~ " " ~
      (!" PHP " ~ AnyChar).rep.! ~ " PHP " ~
      amount ~ eol.rep
  ).map { case (transactionDate, postDate, description, amount) =>
    Transaction(transactionDate, postDate, description, amount)
  }

  val DateFmt = DateTimeFormatter.ofPattern("MM/dd/yy")
  def date[T: P] = P(
    (digit ~ digit ~ "/" ~ digit ~ digit ~ "/" ~ digit ~ digit).!
  ).map(LocalDate.parse(_, DateFmt))
}
