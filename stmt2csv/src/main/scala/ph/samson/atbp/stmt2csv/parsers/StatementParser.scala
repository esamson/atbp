package ph.samson.atbp.stmt2csv.parsers

import better.files.File
import org.apache.pdfbox.Loader
import org.apache.pdfbox.pdmodel.PDDocument
import ph.samson.atbp.stmt2csv.CsvEntry
import zio.Task
import zio.ZIO

trait StatementParser extends (File => Task[List[CsvEntry]]) {

  def extractText(doc: PDDocument): Task[String]

  def parseEntries(text: String): Task[List[CsvEntry]]

  final override def apply(source: File): Task[List[CsvEntry]] = {
    def acquire = ZIO.attemptBlockingIO(Loader.loadPDF(source.toJava))
    def release(doc: PDDocument) = ZIO.succeedBlocking(doc.close())

    for {
      text <- ZIO.acquireReleaseWith(acquire)(release)(extractText)
      entries <- parseEntries(text)
    } yield entries
  }
}
