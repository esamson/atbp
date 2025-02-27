package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.markdown.MarkdownParser
import com.atlassian.adf.model.node.Doc
import zio.Config
import zio.ConfigProvider
import zio.Task
import zio.ZIO
import zio.config.magnolia.*
import zio.config.yaml.*

import java.nio.charset.StandardCharsets
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.util.Base64
import scala.jdk.CollectionConverters.*

object Parser {
  val Descriptor: Config[FrontMatter] = deriveConfig[FrontMatter]

  val Delimiter = "---\n"

  case class Parsed(
      frontMatter: FrontMatter,
      doc: Doc,
      contentHash: String
  )

  case class FrontMatter(
      title: Option[String] = None
  )

  object FrontMatter {
    val Empty: FrontMatter = FrontMatter(None)

    def apply(title: String): FrontMatter =
      FrontMatter(Some(title))
  }

  def parse(source: File) = {
    val p = for {
      content <- ZIO.attemptBlockingIO(source.contentAsString)
      (frontMatter, markdown) = splitFrontMatter(content)
      fm <- parseFrontMatter(frontMatter)
      doc <- parseMarkdown(markdown)
      contentHash <- computeHash(markdown)
    } yield {
      Parsed(fm, doc, contentHash)
    }

    p.mapError(cause => ParsingFailed(source, cause))
  }

  def splitFrontMatter(content: String): (String, String) = {
    if (content.startsWith(Delimiter)) {
      val endPos = content.indexOf(Delimiter, Delimiter.length)
      if (endPos == -1) {
        "" -> content
      } else {
        content.substring(Delimiter.length, endPos).trim ->
          content.substring(endPos + Delimiter.length)
      }
    } else {
      "" -> content
    }
  }

  def parseFrontMatter(frontMatter: String): Task[FrontMatter] = for {
    source <- ConfigProvider.fromYamlStringZIO(frontMatter)
    info <- source.load(Descriptor)
  } yield {
    info
  }

  def parseMarkdown(markdown: String): Task[Doc] = ZIO.attempt {
    val mp = new MarkdownParser
    mp.unmarshall(markdown)
  }

  def computeHash(content: String): Task[String] = ZIO.attempt {
    Base64.getUrlEncoder.encodeToString {
      MessageDigest.getInstance("SHA-1").digest(content.getBytes(UTF_8))
    }
  }

  case class ParsingFailed(source: File, cause: Throwable)
      extends Exception(s"Parsing failed: $source", cause)
}
