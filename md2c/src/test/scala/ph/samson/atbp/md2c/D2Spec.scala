package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.model.node.AbstractContentNode
import com.atlassian.adf.model.node.CodeBlock
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Media.ExternalMedia
import zio.test.*

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.jdk.CollectionConverters.*
import scala.jdk.StreamConverters.*

object D2Spec extends ZIOSpecDefault {

  private val shapes =
    """```d2
      |x -> y
      |```""".stripMargin

  private def externalMediaWithSuffix(
      doc: Doc,
      suffix: String
  ): List[ExternalMedia] =
    doc
      .allNodesOfType(classOf[ExternalMedia])
      .toScala(List)
      .filter { media =>
        val path = media.url()
        val file = File(path)
        file.exists && file.size > 0 && path.endsWith(suffix)
      }

  private def pngImage(media: ExternalMedia): BufferedImage =
    ImageIO.read(File(media.url()).toJava)

  private def textSiblingAfter(
      doc: Doc,
      d2Block: CodeBlock
  ): Option[CodeBlock] = {
    val containers = doc
      .findMatchingDescendants(
        classOf[CodeBlock],
        (cb: CodeBlock) => cb == d2Block
      )
      .asScala
      .toList
    containers.headOption.flatMap { container =>
      val parent = container.parent()
      val index = container.children().get(0).index()
      parent match {
        case content: AbstractContentNode[?, ?] =>
          val siblings = content.content()
          if (index + 1 < siblings.size) {
            Option(siblings.get(index + 1)).collect { case cb: CodeBlock => cb }
          } else {
            None
          }
        case _ =>
          None
      }
    }
  }

  override def spec = suite("D2")(
    suite("with d2 available")(
      test("default d2 fence renders to PNG MediaSingle") {
        for {
          doc <- Parser.parseMarkdown(shapes)
          transformed <- D2.transform(doc)
        } yield {
          val d2CodeBlocks = transformed
            .allNodesOfType(classOf[CodeBlock])
            .filter(_.language().orElse("").startsWith("d2"))
            .toScala(List)
          val pngs = externalMediaWithSuffix(transformed, ".d2.png")
          val image = pngImage(pngs.head)
          assertTrue(
            d2CodeBlocks.isEmpty,
            pngs.nonEmpty,
            // Guard against a degenerate/blank raster.
            image.getWidth > 8,
            image.getHeight > 8
          )
        }
      }
    ).whenZIO(D2.d2Available),
    test("missing binary keeps CodeBlock with NotOnPathMessage sibling") {
      for {
        doc <- Parser.parseMarkdown(shapes)
        originalSource = doc
          .allNodesOfType(classOf[CodeBlock])
          .filter(_.language().orElse("").startsWith("d2"))
          .toScala(List)
          .head
          .toPlainText
        renders <- D2.render(doc, executable = "definitely-not-a-real-d2")
        transformed <- D2.transform(doc, "definitely-not-a-real-d2")
      } yield {
        val d2CodeBlocks = transformed
          .allNodesOfType(classOf[CodeBlock])
          .filter(_.language().orElse("").startsWith("d2"))
          .toScala(List)
        val externalMedia = transformed
          .allNodesOfType(classOf[ExternalMedia])
          .toScala(List)
        val block = d2CodeBlocks.head
        val sibling = textSiblingAfter(transformed, block).get
        assertTrue(
          renders.values.forall(_ == Left(D2.NotOnPathMessage)),
          d2CodeBlocks.size == 1,
          block.language().orElse("") == "d2",
          block.toPlainText == s"${D2.RenderFailureComment}\n$originalSource",
          externalMedia.isEmpty,
          sibling.language().orElse("") == "text",
          sibling.toPlainText ==
            s"${D2.RenderFailureDetailsHeader}\n${D2.NotOnPathMessage}"
        )
      }
    }
  )
}
