package ph.samson.atbp.stmt2csv.parsers

import fastparse.*

import NoWhitespace.*

def longMonth[T: P] = P(
  IgnoreCase("January") |
    IgnoreCase("February") |
    IgnoreCase("March") |
    IgnoreCase("April") |
    IgnoreCase("May") |
    IgnoreCase("June") |
    IgnoreCase("July") |
    IgnoreCase("August") |
    IgnoreCase("September") |
    IgnoreCase("October") |
    IgnoreCase("November") |
    IgnoreCase("December")
)

def shortMonth[T: P] = P(
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

def shortDay[T: P] = P(digit ~ digit.?)

def year[T: P] = P(digit ~ digit ~ digit ~ digit)

def amount[T: P] =
  P(
    (("-" ~ " ".?).? ~
      digit.rep(1) ~
      ("," ~ digit ~ digit ~ digit).rep ~
      "." ~ digit ~ digit).!
  ).map(amount => BigDecimal(amount.replace(",", "").replace(" ", "")))

def digit[T: P] = P(CharIn("0-9"))

def camel(s: String): String = s"${s.head.toUpper}${s.tail.toLowerCase()}"

def anyLine[T: P] = P((!eol ~ AnyChar).rep.! ~ eol)

def eol[T: P] = P("\n" | "\r\n" | "\r" | "\f")
