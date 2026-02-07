package ph.samson.atbp.stmt2csv.parsers

import fastparse.*
import org.apache.pdfbox.text.PDFTextStripper
import ph.samson.atbp.stmt2csv.CsvEntry
import zio.Task
import zio.ZIO

import java.time.LocalDate
import java.time.MonthDay
import java.time.format.DateTimeFormatter

import NoWhitespace.*

object BpiCreditCardParser extends StatementParser {

  case class Statement(
      date: LocalDate,
      transactions: List[Transaction]
  )

  sealed abstract class Line
  case class Boring(value: String) extends Line
  case class StatementDate(date: LocalDate) extends Line
  case class Transaction(
      date: MonthDay,
      posted: MonthDay,
      description: String,
      amount: BigDecimal
  ) extends Line

  override def stripper: PDFTextStripper = {
    val s = new PDFTextStripper
    s.setAverageCharTolerance(0.9f)
    s.setSpacingTolerance(1.5f)
    s
  }

  override def validate(text: String): Task[String] = if (
    text.contains("BPI Credit Card")
  ) {
    ZIO.succeed(text)
  } else {
    ZIO.fail(new IllegalArgumentException("Not a BPI Credit Card Statement"))
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

    stmt.transactions.map {
      case Transaction(date, posted, description, amount) =>
        CsvEntry(fix(date), description, amount)
    }
  }

  def statement[T: P]: P[Statement] = P(
    (!statementDate ~ anyLine).rep ~
      statementDate ~
      (transaction | anyLine.map(Boring.apply)).rep
  ).map { case (_, date, lines) =>
    Statement(
      date.date,
      lines.toList.collect { case t: Transaction =>
        t
      }
    )
  }

  def statementDate[T: P] =
    P("STATEMENT DATE " ~ longDate ~ eol.rep).map(StatementDate.apply)

  val LongDateFmt = DateTimeFormatter.ofPattern("MMMM d, yyyy")
  def longDate[T: P] =
    P((longMonth ~ " " ~ shortDay ~ ", " ~ year).!).map(s =>
      LocalDate.parse(camel(s), LongDateFmt)
    )

  val LongDayFmt = DateTimeFormatter.ofPattern("MMMM d")
  def longMonthDay[T: P] =
    P((longMonth ~ " " ~ shortDay).!).map((s => MonthDay.parse(s, LongDayFmt)))

  def transaction[T: P] =
    P(
      longMonthDay ~ " " ~
        longMonthDay ~ " " ~
        (!(amount ~ eol) ~ AnyChar).rep.! ~
        amount ~
        eol.rep(1)
    ).map({ case (t, p, d, a) =>
      Transaction(
        date = t,
        posted = p,
        description = d.replace("\n", " - ").trim,
        amount = a
      )
    })
}
