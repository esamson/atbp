package ph.samson.atbp.stmt2csv.parsers

import fastparse.*
import org.apache.pdfbox.text.PDFTextStripper
import ph.samson.atbp.stmt2csv.CsvEntry
import zio.Task
import zio.ZIO

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import NoWhitespace.*

object BdoCreditCardParser extends StatementParser {

  override def stripper: PDFTextStripper = new PDFTextStripper

  override def validate(text: String): Task[String] =
    if (text.contains("BDO Credit Card")) {
      ZIO.succeed(text)
    } else {
      ZIO.fail(new IllegalArgumentException("Not a BDO Credit Card Statement"))
    }

  override def parseEntries(text: String): Task[List[CsvEntry]] = ZIO.attempt {
    val transactions = parse(text, statement(using _)).get.value
    transactions.toList.map { case Transaction(saleDate, _, details, amount) =>
      CsvEntry(saleDate, details, amount)
    }
  }

  sealed abstract class Line
  case class Boring(value: String) extends Line
  case class Transaction(
      saleDate: LocalDate,
      postDate: LocalDate,
      details: String,
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
      (!(amount ~ eol) ~ AnyChar).rep.! ~
      amount ~ eol ~
      extraDetail.rep ~
      eol.rep
  ).map { case (saleDate, postDate, details, amount, extra) =>
    val description = details.trim :: extra.toList.map(_.trim)
    Transaction(saleDate, postDate, description.mkString(" | "), amount)
  }

  def extraDetail[T: P] = P(
    !("SUBTOTAL" | date) ~ anyLine
  )

  val DateFmt = DateTimeFormatter.ofPattern("MM/dd/yy")
  def date[T: P] = P(
    (digit ~ digit ~ "/" ~ digit ~ digit ~ "/" ~ digit ~ digit).!
  ).map(LocalDate.parse(_, DateFmt))
}
