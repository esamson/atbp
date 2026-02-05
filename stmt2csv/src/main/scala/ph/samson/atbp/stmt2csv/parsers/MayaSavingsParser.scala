package ph.samson.atbp.stmt2csv.parsers

import org.apache.pdfbox.text.PDFTextStripper
import ph.samson.atbp.stmt2csv.CsvEntry
import zio.{Task, ZIO}
import fastparse.*
import NoWhitespace.*

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalTime, MonthDay}

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

  override def stripper: PDFTextStripper = {
    val s = new PDFTextStripper
    s.setSortByPosition(true)
    s
  }

  override def validate(text: String): Task[String] = if (
    text.contains("Maya Bank, Inc.")
  ) {
    println(text)
    ZIO.succeed(text)
  } else {
    ZIO.fail(new IllegalArgumentException("Not a Maya Savings Statement"))
  }

  override def parseEntries(text: String): Task[List[CsvEntry]] = ZIO.attempt {
    val stmt = parse(text, statement(using _)).get.value
    ???
  }

  def statement[T: P]: P[Statement] = P(
    (!statementDate ~ anyLine).rep ~
      statementDate ~
      (transaction | anyLine.map(Boring.apply)).rep
  ).map { case (prelude, date, lines) =>
    println(s"prelude:\n${prelude.mkString("\n")}")
    println(s"lines:\n${lines.mkString("\n")}")
    Statement(date.date, Nil)
  }

  def statementDate[T:P] = P(
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
      amount ~ eol.rep
  ).map {
    case (date, time, description, amount, balance) => 
      Transaction(date, time, description.trim, amount, balance)
  }

  val MonthDayFmt = DateTimeFormatter.ofPattern("d MMM")
  def monthDay[T: P] =
    P((shortDay ~ " " ~ shortMonth).!).map(s => MonthDay.parse(s, MonthDayFmt))

  val TimeFmt = DateTimeFormatter.ofPattern("hh:mm a")
  def time[T: P] =
    P((digit.rep(2) ~ ":" ~ digit.rep(2) ~ (" AM" | " PM")).!).map(s => LocalTime.parse(s, TimeFmt))
}
