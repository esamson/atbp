package ph.samson.atbp.stmt2csv

import java.time.LocalDate

case class CsvEntry(
    date: LocalDate,
    description: String,
    debit: BigDecimal,
    credit: BigDecimal
)
