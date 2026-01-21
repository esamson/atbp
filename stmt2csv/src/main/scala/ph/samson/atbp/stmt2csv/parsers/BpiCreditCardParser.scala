package ph.samson.atbp.stmt2csv.parsers

import org.apache.pdfbox.pdmodel.PDDocument
import ph.samson.atbp.stmt2csv.CsvEntry
import zio.Task

object BpiCreditCardParser extends StatementParser {

  override def extractText(doc: PDDocument): Task[String] = ???

  override def parseEntries(text: String): Task[List[CsvEntry]] = ???
}
