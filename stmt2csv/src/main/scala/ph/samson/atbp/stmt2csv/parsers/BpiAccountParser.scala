package ph.samson.atbp.stmt2csv.parsers

import fastparse.*
import org.apache.pdfbox.text.PDFTextStripper
import ph.samson.atbp.stmt2csv.CsvEntry
import zio.Task
import zio.ZIO

import java.time.LocalDate
import java.time.MonthDay
import java.time.format.DateTimeFormatter
import scala.annotation.tailrec

import NoWhitespace.*

object BpiAccountParser extends StatementParser {

  case class Statement(
      info: StatementInfo,
      beginningBalance: BigDecimal,
      transactions: List[Transaction]
  )

  override def stripper: PDFTextStripper = {
    val s = new PDFTextStripper
    s.setAverageCharTolerance(0.9f)
    s.setSpacingTolerance(1.5f)
    s
  }

  override def validate(text: String): Task[String] = if (
    text.contains("www.bpi.com.ph")
  ) {
    ZIO.succeed(text)
  } else {
    ZIO.fail(new IllegalArgumentException("Not a BPI Account Statement"))
  }

  override def parseEntries(text: String): Task[List[CsvEntry]] = ZIO.attempt {
    val stmt = parse(text, statement(using _)).get.value
    convert(stmt)
  }

  def convert(stmt: Statement): List[CsvEntry] = {

    @tailrec
    def doConvert(
        unprocessed: List[Transaction],
        lastBalance: BigDecimal,
        converted: List[CsvEntry]
    ): List[CsvEntry] = unprocessed match {
      case Nil => converted.reverse
      case Transaction(date, description, amount, balance) :: next =>
        val (debit, credit) = if (balance > lastBalance) {
          (BigDecimal(0), amount)
        } else {
          (amount, BigDecimal(0))
        }
        val entry = CsvEntry(fix(date, stmt.info), description, debit, credit)

        doConvert(next, balance, entry :: converted)
    }

    doConvert(stmt.transactions, stmt.beginningBalance, Nil)
  }

  def fix(date: MonthDay, info: StatementInfo): LocalDate = {
    val fromYear = date.atYear(info.from.getYear)
    if (!fromYear.isBefore(info.from) && !fromYear.isAfter(info.to)) {
      fromYear
    } else {
      date.atYear(info.to.getYear)
    }
  }

  def statement[T: P]: P[Statement] = P(
    (!statementInfo ~ anyLine).rep ~
      statementInfo ~
      (!beginningBalance ~ anyLine).rep ~
      beginningBalance ~
      (transaction | anyLine.map(Boring.apply)).rep
  ).map { case (_, info, _, balance, lines) =>
    Statement(
      info,
      balance.amount,
      lines.toList.collect { case t: Transaction =>
        t
      }
    )
  }

  def statementInfo[T: P] =
    P(
      "PERIOD COVERED " ~ date ~ " - " ~ date ~
        " NO: " ~ (!eol ~ AnyChar).rep.! ~ eol.rep
    ).map({ case (from, to, account) =>
      StatementInfo(from, to, account)
    })

  def beginningBalance[T: P] =
    P("BEGINNING BALANCE " ~ amount ~ eol.rep).map(BeginningBalance.apply)

  def transaction[T: P]: P[Transaction] =
    P(
      monthDay ~ " " ~
        (!amount ~ AnyChar).rep.! ~
        amount ~ " " ~
        amount ~ eol.rep
    ).map({ case (date, description, amount, balance) =>
      Transaction(
        date,
        description = description.trim,
        amount = amount,
        balance = balance
      )
    })

  sealed abstract class Line
  case class Boring(s: String) extends Line
  case class StatementInfo(from: LocalDate, to: LocalDate, account: String)
      extends Line
  case class BeginningBalance(amount: BigDecimal) extends Line
  case class Transaction(
      date: MonthDay,
      description: String,
      amount: BigDecimal,
      balance: BigDecimal
  ) extends Line

  val LocalDateFmt = DateTimeFormatter.ofPattern("MMM dd, yyyy")
  def date[T: P] =
    P((month ~ " " ~ day ~ ", " ~ year).!)
      .map(s => LocalDate.parse(camel(s), LocalDateFmt))

  val MonthDayFmt = DateTimeFormatter.ofPattern("MMM dd")
  def monthDay[T: P] =
    P((month ~ " " ~ day).!).map(s => MonthDay.parse(s, MonthDayFmt))

  def month[T: P] = P(
    IgnoreCase("Jan") |
      IgnoreCase("Feb") |
      IgnoreCase("Mar") |
      IgnoreCase("Apr") |
      IgnoreCase("May") |
      IgnoreCase("Jun") |
      IgnoreCase("Jul") |
      IgnoreCase("Aug") |
      IgnoreCase("Sep") |
      IgnoreCase("Oct") |
      IgnoreCase("Nov") |
      IgnoreCase("Dec")
  )
  def day[T: P] = P(digit.rep(min = 1, max = 2))
}
