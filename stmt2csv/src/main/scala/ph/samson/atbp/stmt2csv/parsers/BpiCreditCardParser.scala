package ph.samson.atbp.stmt2csv.parsers

import org.apache.pdfbox.text.PDFTextStripper
import ph.samson.atbp.stmt2csv.CsvEntry
import zio.Task
import zio.ZIO

object BpiCreditCardParser extends StatementParser {

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
    ZIO.fail(new IllegalArgumentException("Not a BPI Statement"))
  }

  override def parseEntries(text: String): Task[List[CsvEntry]] = ???
}
