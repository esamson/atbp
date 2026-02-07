package ph.samson.atbp.stmt2csv.parsers

import fastparse.*
import org.apache.pdfbox.text.PDFTextStripper
import ph.samson.atbp.stmt2csv.CsvEntry
import zio.Task
import zio.ZIO

import java.time.LocalDate
import java.time.LocalTime
import java.time.MonthDay
import java.time.format.DateTimeFormatter

import NoWhitespace.*

object MayaSavingsParser extends StatementParser {

  case class Statement(
      date: LocalDate,
      transactions: List[Transaction]
  )

  sealed abstract class Line
  case class Boring(value: String) extends Line
  case class StatementDate(date: LocalDate) extends Line
  case class Transaction(
      date: MonthDay,
      time: LocalTime,
      description: String,
      amount: BigDecimal,
      balance: BigDecimal
  ) extends Line
  case class ExtraDetail(value: String) extends Line

  override def stripper: PDFTextStripper = {
    val s = new PDFTextStripper
    s.setSortByPosition(true)
    s
  }

  override def validate(text: String): Task[String] = if (
    text.contains("Maya Bank, Inc.")
  ) {
    ZIO.succeed(text)
  } else {
    ZIO.fail(new IllegalArgumentException("Not a Maya Savings Statement"))
  }

  override def parseEntries(text: String): Task[List[CsvEntry]] = ZIO.attempt {
    val stmt = parse(text, statement(using _)).get.value
    convert(stmt)
  }

  def convert(stmt: Statement): List[CsvEntry] = {

    def fix(date: MonthDay): LocalDate = {
      val atYear = date.atYear(stmt.date.getYear)
      if (atYear.isAfter(stmt.date)) {
        atYear.minusYears(1)
      } else {
        atYear
      }
    }

    stmt.transactions.map { case Transaction(date, _, description, amount, _) =>
      CsvEntry(fix(date), description, amount)
    }
  }

  def statement[T: P]: P[Statement] = P(
    (!statementDate ~ anyLine).rep ~
      statementDate ~
      (transaction | anyLine.map(Boring.apply)).rep
  ).map { case (prelude, date, lines) =>
    Statement(date.date, lines.toList.collect { case t: Transaction => t })
  }

  def statementDate[T: P] = P(
    "Statement date: " ~ longDate ~ eol.rep
  ).map(StatementDate.apply)

  val LongDateFmt = DateTimeFormatter.ofPattern("d MMMM yyyy")
  def longDate[T: P] =
    P((shortDay ~ " " ~ longMonth ~ " " ~ year).!).map(s =>
      LocalDate.parse(s, LongDateFmt)
    )

  def transaction[T: P]: P[Transaction] = P(
    monthDay ~ ", " ~
      time ~ " " ~
      (!(amount ~ " " ~ amount ~ eol) ~ AnyChar).rep.! ~
      amount ~ " " ~
      amount ~ eol ~
      extraDetail.rep ~
      eol.rep
  ).map { case (date, time, mainDesc, amount, balance, extraDesc) =>
    val description = mainDesc.trim :: extraDesc.toList.map(_.value.trim)
    Transaction(date, time, description.mkString(" | "), amount, balance)
  }

  def extraDetail[T: P] = P(
    !("Page" | monthDay) ~ anyLine
  ).map(ExtraDetail.apply)

  val MonthDayFmt = DateTimeFormatter.ofPattern("d MMM")
  def monthDay[T: P] =
    P((shortDay ~ " " ~ shortMonth).!).map(s => MonthDay.parse(s, MonthDayFmt))

  val TimeFmt = DateTimeFormatter.ofPattern("hh:mm a")
  def time[T: P] =
    P((digit.rep(2) ~ ":" ~ digit.rep(2) ~ (" AM" | " PM")).!).map(s =>
      LocalTime.parse(s, TimeFmt)
    )
}
