package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.model.node.AbstractContentNode
import com.atlassian.adf.model.node.CodeBlock
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Media.ExternalMedia
import com.atlassian.adf.model.node.Panel
import zio.test.*

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.jdk.CollectionConverters.*
import scala.jdk.StreamConverters.*

object MermaidSpec extends ZIOSpecDefault {

  private val sequenceDiagram =
    """```mermaid
      |sequenceDiagram
      |    Alice->>Bob: Hello
      |```""".stripMargin

  private def mermaidFence(language: String): String =
    s"""```$language
       |sequenceDiagram
       |    Alice->>Bob: Hello
       |```""".stripMargin

  /** Invalid diagram: mermaid-java returns !isSuccessful for this input. */
  private val invalidMermaidSource = "invalid!!!"

  private def nullMermaidFence(language: String): String =
    s"""```$language
       |$invalidMermaidSource
       |```""".stripMargin

  private def leftErrorMessage(
      renders: Map[Mermaid.RenderKey, Mermaid.RenderOutcome]
  ): String =
    renders.values.collectFirst { case Left(result) =>
      result.getErrorMessage()
    }.get

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

  /** Batik blank renders autoCrop to 8×8; real diagrams are much larger. */
  private def pngImage(media: ExternalMedia): BufferedImage =
    ImageIO.read(File(media.url()).toJava)

  private def textSiblingAfter(
      doc: Doc,
      mermaidBlock: CodeBlock
  ): Option[CodeBlock] = {
    val containers = doc
      .findMatchingDescendants(
        classOf[CodeBlock],
        (cb: CodeBlock) => cb == mermaidBlock
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

  override def spec = suite("Mermaid")(
    test("default mermaid fence renders to PNG MediaSingle") {
      for {
        doc <- Parser.parseMarkdown(sequenceDiagram)
        transformed <- Mermaid.transform(doc)
      } yield {
        val mermaidCodeBlocks = transformed
          .allNodesOfType(classOf[CodeBlock])
          .filter(_.language().orElse("").startsWith("mermaid"))
          .toScala(List)
        val pngs = externalMediaWithSuffix(transformed, ".png")
        val image = pngImage(pngs.head)
        assertTrue(
          mermaidCodeBlocks.isEmpty,
          pngs.nonEmpty,
          // Guard empty Batik raster (autoCrop of blank SVG → 8×8, ~70 bytes)
          image.getWidth > 8,
          image.getHeight > 8
        )
      }
    },
    test("mermaid.png fence renders to PNG MediaSingle") {
      for {
        doc <- Parser.parseMarkdown(mermaidFence("mermaid.png"))
        transformed <- Mermaid.transform(doc)
      } yield {
        val mermaidCodeBlocks = transformed
          .allNodesOfType(classOf[CodeBlock])
          .filter(_.language().orElse("").startsWith("mermaid"))
          .toScala(List)
        assertTrue(
          mermaidCodeBlocks.isEmpty,
          externalMediaWithSuffix(transformed, ".png").nonEmpty
        )
      }
    },
    test("mermaid.svg fence renders to SVG MediaSingle") {
      for {
        doc <- Parser.parseMarkdown(mermaidFence("mermaid.svg"))
        transformed <- Mermaid.transform(doc)
      } yield {
        val mermaidCodeBlocks = transformed
          .allNodesOfType(classOf[CodeBlock])
          .filter(_.language().orElse("").startsWith("mermaid"))
          .toScala(List)
        assertTrue(
          mermaidCodeBlocks.isEmpty,
          externalMediaWithSuffix(transformed, ".svg").nonEmpty
        )
      }
    },
    test("same source mermaid and mermaid.svg keep distinct formats") {
      val markdown =
        s"""${mermaidFence("mermaid")}
           |
           |${mermaidFence("mermaid.svg")}""".stripMargin
      for {
        doc <- Parser.parseMarkdown(markdown)
        transformed <- Mermaid.transform(doc)
      } yield {
        val pngs = externalMediaWithSuffix(transformed, ".mermaid.png")
        val svgs = externalMediaWithSuffix(transformed, ".mermaid.svg")
        assertTrue(
          pngs.nonEmpty,
          svgs.nonEmpty,
          pngs.head.url() != svgs.head.url()
        )
      }
    },
    test("staging replaces mermaid and plantuml fences") {
      for {
        source <- SourceTreeSpec.from("Mermaid Sequence")
        staged <- StagedTree.from(source)
      } yield {
        val doc = staged.root.adf
        val codeBlocks = doc
          .allNodesOfType(classOf[CodeBlock])
          .toScala(List)
        val languages = codeBlocks.map(_.language().orElse(""))
        assertTrue(
          !languages.exists(_.startsWith("mermaid")),
          !languages.exists(_.startsWith("plantuml")),
          externalMediaWithSuffix(doc, ".mermaid.png").nonEmpty,
          externalMediaWithSuffix(doc, ".plantuml.png").nonEmpty
        )
      }
    },
    test("staging keeps failed mermaid as CodeBlock with error sibling") {
      for {
        source <- SourceTreeSpec.from("Mermaid Failure")
        parsed <- Parser.parse(
          TestFiles("trees") / "Mermaid Failure" / "Mermaid Failure.md"
        )
        originalSource = parsed.doc
          .allNodesOfType(classOf[CodeBlock])
          .filter(_.language().orElse("").startsWith("mermaid"))
          .toScala(List)
          .head
          .toPlainText
        renders <- Mermaid.render(parsed.doc)
        expectedError = leftErrorMessage(renders)
        staged <- StagedTree.from(source)
      } yield {
        val doc = staged.root.adf
        val mermaidCodeBlocks = doc
          .allNodesOfType(classOf[CodeBlock])
          .filter(_.language().orElse("").startsWith("mermaid"))
          .toScala(List)
        val externalMedia = doc
          .allNodesOfType(classOf[ExternalMedia])
          .toScala(List)
        val block = mermaidCodeBlocks.head
        val sibling = textSiblingAfter(doc, block).get
        assertTrue(
          mermaidCodeBlocks.size == 1,
          block.language().orElse("") == "mermaid",
          block.toPlainText == s"${Mermaid.RenderFailureComment}\n$originalSource",
          externalMedia.isEmpty,
          sibling.language().orElse("") == "text",
          sibling.toPlainText ==
            s"${Mermaid.RenderFailureDetailsHeader}\n$expectedError"
        )
      }
    },
    test("invalid mermaid fence keeps CodeBlock with failure comment (PNG)") {
      for {
        doc <- Parser.parseMarkdown(nullMermaidFence("mermaid"))
        originalSource = doc
          .allNodesOfType(classOf[CodeBlock])
          .filter(_.language().orElse("").startsWith("mermaid"))
          .toScala(List)
          .head
          .toPlainText
        renders <- Mermaid.render(doc)
        expectedError = leftErrorMessage(renders)
        transformed <- Mermaid.transform(doc)
      } yield {
        val mermaidCodeBlocks = transformed
          .allNodesOfType(classOf[CodeBlock])
          .filter(_.language().orElse("").startsWith("mermaid"))
          .toScala(List)
        val externalMedia = transformed
          .allNodesOfType(classOf[ExternalMedia])
          .toScala(List)
        val block = mermaidCodeBlocks.head
        val sibling = textSiblingAfter(transformed, block).get
        assertTrue(
          mermaidCodeBlocks.size == 1,
          block.language().orElse("") == "mermaid",
          block.toPlainText == s"${Mermaid.RenderFailureComment}\n$originalSource",
          externalMedia.isEmpty,
          sibling.language().orElse("") == "text",
          sibling.toPlainText ==
            s"${Mermaid.RenderFailureDetailsHeader}\n$expectedError"
        )
      }
    },
    test(
      "invalid mermaid.svg fence keeps CodeBlock with failure comment (SVG)"
    ) {
      for {
        doc <- Parser.parseMarkdown(nullMermaidFence("mermaid.svg"))
        originalSource = doc
          .allNodesOfType(classOf[CodeBlock])
          .filter(_.language().orElse("").startsWith("mermaid"))
          .toScala(List)
          .head
          .toPlainText
        renders <- Mermaid.render(doc)
        expectedError = leftErrorMessage(renders)
        transformed <- Mermaid.transform(doc)
      } yield {
        val mermaidCodeBlocks = transformed
          .allNodesOfType(classOf[CodeBlock])
          .filter(_.language().orElse("").startsWith("mermaid"))
          .toScala(List)
        val externalMedia = transformed
          .allNodesOfType(classOf[ExternalMedia])
          .toScala(List)
        val block = mermaidCodeBlocks.head
        val sibling = textSiblingAfter(transformed, block).get
        assertTrue(
          mermaidCodeBlocks.size == 1,
          block.language().orElse("") == "mermaid.svg",
          block.toPlainText == s"${Mermaid.RenderFailureComment}\n$originalSource",
          externalMedia.isEmpty,
          sibling.language().orElse("") == "text",
          sibling.toPlainText ==
            s"${Mermaid.RenderFailureDetailsHeader}\n$expectedError"
        )
      }
    },
    test("invalid mermaid in nested panel inserts sibling text block") {
      val doc = Doc.doc(
        Panel.info(
          CodeBlock.codeBlock(invalidMermaidSource).language("mermaid")
        )
      )

      for {
        renders <- Mermaid.render(doc)
        expectedError = leftErrorMessage(renders)
        transformed <- Mermaid.transform(doc)
      } yield {
        val mermaidCodeBlocks = transformed
          .allNodesOfType(classOf[CodeBlock])
          .filter(_.language().orElse("").startsWith("mermaid"))
          .toScala(List)
        val externalMedia = transformed
          .allNodesOfType(classOf[ExternalMedia])
          .toScala(List)
        val block = mermaidCodeBlocks.head
        val sibling = textSiblingAfter(transformed, block).get
        val panel = transformed
          .allNodesOfType(classOf[Panel])
          .toScala(List)
          .head
        assertTrue(
          mermaidCodeBlocks.size == 1,
          block.language().orElse("") == "mermaid",
          block.toPlainText ==
            s"${Mermaid.RenderFailureComment}\n$invalidMermaidSource",
          externalMedia.isEmpty,
          sibling.language().orElse("") == "text",
          sibling.toPlainText ==
            s"${Mermaid.RenderFailureDetailsHeader}\n$expectedError",
          panel.allNodesOfType(classOf[CodeBlock]).count() == 2
        )
      }
    },
    test("non-mermaid CodeBlocks are unchanged") {
      val markdown =
        """```scala
          |val x = 1
          |```
          |
          |```plantuml
          |Alice -> Bob: hi
          |```""".stripMargin

      for {
        doc <- Parser.parseMarkdown(markdown)
        transformed <- Mermaid.transform(doc)
      } yield {
        val codeBlocks = transformed
          .allNodesOfType(classOf[CodeBlock])
          .toScala(List)
        val languages = codeBlocks.map(_.language().orElse(""))
        val externalMedia = transformed
          .allNodesOfType(classOf[ExternalMedia])
          .toScala(List)
        assertTrue(
          languages.contains("scala"),
          languages.contains("plantuml"),
          externalMedia.isEmpty
        )
      }
    }
  )
}
