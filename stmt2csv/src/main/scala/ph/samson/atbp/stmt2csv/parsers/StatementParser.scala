package ph.samson.atbp.stmt2csv.parsers

import better.files.File
import org.apache.pdfbox.Loader
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.text.PDFTextStripper
import ph.samson.atbp.stmt2csv.CsvEntry
import zio.Task
import zio.ZIO

trait StatementParser extends (File => Task[List[CsvEntry]]) {

  def stripper: PDFTextStripper

  def validate(text: String): Task[String]

  def parseEntries(text: String): Task[List[CsvEntry]]

  private def extract(doc: PDDocument): Task[String] = for {
    text <- ZIO.attempt(stripper.getText(doc))
    validated <- validate(text)
  } yield {
    validated
  }

  final def extractText(source: File): Task[String] = {
    def acquire = ZIO.attemptBlockingIO(Loader.loadPDF(source.toJava))
    def release(doc: PDDocument) = ZIO.succeedBlocking(doc.close())

    for {
      text <- ZIO.acquireReleaseWith(acquire)(release)(extract)
    } yield text
  }

  final override def apply(source: File): Task[List[CsvEntry]] = {
    for {
      text <- extractText(source)
      entries <- parseEntries(text)
    } yield entries
  }
}
