package ph.samson.atbp.c2md

import dev.nthings.adf4j.AdfToMarkdown
import dev.nthings.adf4j.confluence.ConfluenceRenderContext
import dev.nthings.adf4j.metadata.AttachmentReference
import dev.nthings.adf4j.options.MarkdownOptions
import dev.nthings.adf4j.result.Diagnostic
import dev.nthings.adf4j.result.MarkdownResult
import ph.samson.atbp.confluence.Client
import ph.samson.atbp.confluence.model.Attachment
import zio.Task
import zio.ZIO
import zio.ZLayer

import scala.jdk.CollectionConverters.*

trait Converter {
  def convert(pageId: String): Task[String]
}

object Converter {
  val AtlasDocFormat = "atlas_doc_format"

  private class LiveImpl(client: Client, adf: AdfToMarkdown) extends Converter {
    override def convert(pageId: String): Task[String] =
      ZIO.logSpan("convert") {
        for {
          page <- client.getPage(pageId, Some(AtlasDocFormat))
          adfJson <- ZIO
            .fromOption(page.body.atlas_doc_format.map(_.value))
            .orElseFail(
              new NoSuchElementException(s"Page $pageId has no ADF body")
            )
          attachments <- client.getPageAttachments(pageId)
          options = optionsFor(attachments)
          result <- ZIO.attempt(adf.convert(adfJson, options))
          _ <- logLossy(pageId, result)
          markdown = Format.withFrontMatter(page.title, result.body())
          _ <- ZIO.logInfo(s"Converted page $pageId: ${page.title}")
        } yield markdown
      }

    private def optionsFor(attachments: List[Attachment]): MarkdownOptions = {
      val refs = attachments.map { attachment =>
        new AttachmentReference(
          attachment.fileId,
          attachment.title,
          attachment.mediaType
        )
      }.asJava
      val urls = attachmentUrls(attachments)

      MarkdownOptions
        .defaults()
        .withConfluenceContext(
          ConfluenceRenderContext.empty().withAttachmentReferences(refs)
        )
        .withAttachmentResolver(ref => urls.getOrElse(ref.fileId(), ""))
        .withMediaResolver(attrs =>
          Option(attrs.id()).flatMap(urls.get).getOrElse("")
        )
    }

    private def attachmentUrls(attachments: List[Attachment]): Map[String, String] =
      attachments.map { attachment =>
        attachment.fileId -> absoluteUrl(attachment.downloadLink)
      }.toMap

    private def absoluteUrl(path: String): String = {
      val normalized = path.stripPrefix("/wiki")
      client.resolveUrl(normalized).toString
    }

    private def logLossy(pageId: String, result: MarkdownResult): Task[Unit] =
      ZIO.when(result.wasLossy()) {
        val messages = result.diagnostics().asScala.collect {
          case diagnostic
              if diagnostic.severity() != Diagnostic.Severity.INFO =>
            s"[${diagnostic.code()}] ${diagnostic.message()}"
        }
        ZIO.logWarning(
          s"Lossy conversion for page $pageId:\n  ${messages.mkString("\n  ")}"
        )
      }.unit
  }

  val layer: ZLayer[Client, Nothing, Converter] = ZLayer {
    ZIO.serviceWith[Client] { client =>
      LiveImpl(client, AdfToMarkdown.create()): Converter
    }
  }
}
