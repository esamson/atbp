package ph.samson.atbp.md2c

import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Expand
import com.atlassian.adf.model.node.Paragraph
import zio.Task

import scala.jdk.CollectionConverters.*

object Footer {
  val Title = "Don't edit here!"
  val Body = "This page was generated from markdown sources."

  def append(doc: Doc): Doc = {
    val content = doc.content().asScala.toList
    val footer = Expand.expand(Paragraph.p(Body)).title(Title)
    Doc.doc((content :+ footer).asJava)
  }

  def contentHash(parserHash: String): Task[String] =
    Parser.computeHash(s"$parserHash\n$Title\n$Body")
}
