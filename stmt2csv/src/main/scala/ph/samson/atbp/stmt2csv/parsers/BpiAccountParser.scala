package ph.samson.atbp.stmt2csv.parsers

import fastparse.*
import ph.samson.atbp.stmt2csv.CsvEntry
import zio.Task
import zio.ZIO

import java.time.MonthDay
import java.time.format.DateTimeFormatter

import NoWhitespace.*

object BpiAccountParser {

  case class Statement(
      beginningBalance: BigDecimal,
      transactions: List[Transaction]
  )

  def apply(text: String): Task[List[CsvEntry]] = ZIO.attempt {
    val stmt = parse(text, statement(using _)).get.value
    convert(stmt)
  }

  def convert(stmt: Statement): List[CsvEntry] = {
    println(
      s"convert: ${stmt.beginningBalance}\n${stmt.transactions.mkString("\n")}"
    )

    Nil
  }

  def statement[T: P]: P[Statement] = P(
    (!beginningBalance ~ anyLine).rep ~
      beginningBalance ~
      (transaction | anyLine.map(Boring.apply)).rep
  ).map { case (_, balance, lines) =>
    Statement(
      balance.amount,
      lines.toList.collect { case t: Transaction =>
        t
      }
    )
  }

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
  case class BeginningBalance(amount: BigDecimal) extends Line
  case class Transaction(
      date: MonthDay,
      description: String,
      amount: BigDecimal,
      balance: BigDecimal
  ) extends Line

  def amount[T: P] =
    P(
      ("-".? ~
        digit.rep(1) ~
        ("," ~ digit ~ digit ~ digit).rep ~
        "." ~ digit ~ digit).!
    ).map(amount => BigDecimal(amount.replace(",", "")))

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

  def digit[T: P] = P(CharIn("0-9"))

  def anyLine[T: P] = P((!eol ~ AnyChar).rep.! ~ eol)

  def eol[T: P] = P("\n" | "\r\n" | "\r" | "\f")
}
