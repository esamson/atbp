package ph.samson.atbp.stmt2csv

import better.files.File
import org.apache.pdfbox.Loader
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.text.PDFTextStripper
import ph.samson.atbp.stmt2csv.parsers.BpiAccountParser
import zio.Task
import zio.ZIO

object Extractor {

  def extract(source: File, target: File): Task[File] = for {
    _ <- ZIO.logInfo(s"loading $source (${source.size()} bytes)")
    doc <- ZIO.attemptBlockingIO(Loader.loadPDF(source.toJava))
    text <- bpiExtract(doc)
    parsed <- BpiAccountParser(text)
    _ <- ZIO.logDebug(s"result:\n$parsed")
  } yield {
    target
  }

  def bpiExtract(doc: PDDocument): Task[String] = for {
    text <- ZIO.attempt {
      val stripper = new PDFTextStripper
      stripper.setAverageCharTolerance(0.9f)
      stripper.setSpacingTolerance(1.5f)

      stripper.getText(doc)
    }
    _ <- ZIO.debug(text)
    bpiText <-
      if (text.contains("www.bpi.com.ph")) {
        ZIO.succeed(text)
      } else {
        ZIO.fail(new IllegalArgumentException("Not a BPI Statement"))
      }
  } yield {
    bpiText
  }
}
