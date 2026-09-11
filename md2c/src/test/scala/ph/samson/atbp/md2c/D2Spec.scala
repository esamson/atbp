package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.model.node.AbstractContentNode
import com.atlassian.adf.model.node.CodeBlock
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Media.ExternalMedia
import com.atlassian.adf.model.node.Panel
import zio.ZIO
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

  private def d2Fence(language: String): String =
    s"""```$language
       |x -> y
       |```""".stripMargin

  private val d2AndSvgFences =
    """```d2
      |x -> y
      |```
      |
      |```d2.svg
      |x -> y
      |```""".stripMargin

  private val missingBinary = "definitely-not-a-real-d2"

  /** A file that exists where `d2` is expected, so `onPath` sees it, but that
    * `ProcessBuilder.start()` still cannot run or that quits without reading
    * its input.
    */
  private def stubBinary(name: String, script: Option[String]) =
    ZIO.attemptBlocking {
      val file = File.newTemporaryFile(name, ".sh")
      script match {
        case Some(body) =>
          file.writeText(body)
          file.toJava.setExecutable(true)
        case None =>
          file.writeText("#!/bin/sh\nexit 0\n")
          file.toJava.setExecutable(false)
      }
      file
    }

  private def leftErrorMessage(
      renders: Map[D2.RenderKey, D2.RenderOutcome]
  ): String =
    renders.values.collectFirst { case Left(message) => message }.get

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

  /** Failure path driven through the missing-binary seam: keeps the `CodeBlock`
    * with its original fence language, body replaced by the failure comment
    * plus the original source, and no `ExternalMedia`.
    */
  private def failureKeepsCodeBlock(language: String) =
    for {
      doc <- Parser.parseMarkdown(d2Fence(language))
      originalSource = doc
        .allNodesOfType(classOf[CodeBlock])
        .filter(_.language().orElse("").startsWith("d2"))
        .toScala(List)
        .head
        .toPlainText
      transformed <- D2.transform(doc, missingBinary)
    } yield {
      val d2CodeBlocks = transformed
        .allNodesOfType(classOf[CodeBlock])
        .filter(_.language().orElse("").startsWith("d2"))
        .toScala(List)
      val externalMedia = transformed
        .allNodesOfType(classOf[ExternalMedia])
        .toScala(List)
      val block = d2CodeBlocks.head
      assertTrue(
        d2CodeBlocks.size == 1,
        block.language().orElse("") == language,
        block.toPlainText == s"${D2.RenderFailureComment}\n$originalSource",
        externalMedia.isEmpty
      )
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
      },
      test("d2.png fence renders to a non-empty .d2.png file") {
        for {
          doc <- Parser.parseMarkdown(d2Fence("d2.png"))
          transformed <- D2.transform(doc)
        } yield {
          val pngs = externalMediaWithSuffix(transformed, ".d2.png")
          assertTrue(pngs.nonEmpty)
        }
      },
      test("d2.svg fence renders to a non-empty .d2.svg file containing <svg") {
        for {
          doc <- Parser.parseMarkdown(d2Fence("d2.svg"))
          transformed <- D2.transform(doc)
        } yield {
          val svgs = externalMediaWithSuffix(transformed, ".d2.svg")
          val contents =
            svgs.headOption.map(media => File(media.url()).contentAsString)
          assertTrue(
            svgs.nonEmpty,
            contents.exists(_.contains("<svg"))
          )
        }
      },
      test(
        "same source in d2 and d2.svg fences yields two distinct files by format"
      ) {
        for {
          doc <- Parser.parseMarkdown(d2AndSvgFences)
          transformed <- D2.transform(doc)
        } yield {
          val pngs = externalMediaWithSuffix(transformed, ".d2.png")
          val svgs = externalMediaWithSuffix(transformed, ".d2.svg")
          assertTrue(
            pngs.size == 1,
            svgs.size == 1,
            pngs.head.url() != svgs.head.url()
          )
        }
      },
      test("duplicate identical fences render once and share one file") {
        val twice = s"$shapes\n\n$shapes"
        for {
          doc <- Parser.parseMarkdown(twice)
          renders <- D2.render(doc)
          transformed <- D2.transform(doc)
        } yield {
          val urls = transformed
            .allNodesOfType(classOf[ExternalMedia])
            .toScala(List)
            .map(_.url())
          val siblingPngs = File(urls.head).parent.list
            .filter(_.name.endsWith(".d2.png"))
            .size
          assertTrue(
            // One key, so no render can overwrite another's outcome.
            renders.size == 1,
            urls.size == 2,
            urls.distinct.size == 1,
            // Proof only one subprocess ran: no fig-2 next to fig-1.
            siblingPngs == 1
          )
        }
      },
      test("staging replaces d2, plantuml and mermaid fences") {
        for {
          source <- SourceTreeSpec.from("D2 Diagram")
          staged <- StagedTree.from(source)
        } yield {
          val doc = staged.root.adf
          val codeBlocks = doc
            .allNodesOfType(classOf[CodeBlock])
            .toScala(List)
          val languages = codeBlocks.map(_.language().orElse(""))
          val externalMedia = doc
            .allNodesOfType(classOf[ExternalMedia])
            .toScala(List)
          assertTrue(
            !languages.exists(_.startsWith("d2")),
            !languages.exists(_.startsWith("plantuml")),
            !languages.exists(_.startsWith("mermaid")),
            externalMedia.size == 3
          )
        }
      },
      test("staging keeps failed d2 as CodeBlock with d2's own stderr") {
        for {
          source <- SourceTreeSpec.from("D2 Failure")
          parsed <- Parser.parse(
            TestFiles("trees") / "D2 Failure" / "D2 Failure.md"
          )
          originalSource = parsed.doc
            .allNodesOfType(classOf[CodeBlock])
            .filter(_.language().orElse("").startsWith("d2"))
            .toScala(List)
            .head
            .toPlainText
          renders <- D2.render(parsed.doc)
          expectedError = leftErrorMessage(renders)
          staged <- StagedTree.from(source)
        } yield {
          val doc = staged.root.adf
          val d2CodeBlocks = doc
            .allNodesOfType(classOf[CodeBlock])
            .filter(_.language().orElse("").startsWith("d2"))
            .toScala(List)
          val externalMedia = doc
            .allNodesOfType(classOf[ExternalMedia])
            .toScala(List)
          val block = d2CodeBlocks.head
          val sibling = textSiblingAfter(doc, block).get
          assertTrue(
            d2CodeBlocks.size == 1,
            block.language().orElse("") == "d2",
            block.toPlainText == s"${D2.RenderFailureComment}\n$originalSource",
            externalMedia.isEmpty,
            sibling.language().orElse("") == "text",
            sibling.toPlainText ==
              s"${D2.RenderFailureDetailsHeader}\n$expectedError"
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
        renders <- D2.render(doc, executable = missingBinary)
        transformed <- D2.transform(doc, missingBinary)
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
    },
    test("d2 present but not runnable is not reported as missing from PATH") {
      for {
        stub <- stubBinary("d2-unrunnable", None)
        doc <- Parser.parseMarkdown(shapes)
        renders <- D2.render(doc, stub.pathAsString)
      } yield {
        val message = leftErrorMessage(renders)
        assertTrue(
          message != D2.NotOnPathMessage,
          message.startsWith("Could not render diagram: "),
          message.contains(stub.name)
        )
      }
    },
    test("d2 quitting without reading a large source is a soft failure") {
      val bigSource =
        (1 to 20000).map(i => s"n$i -> n${i + 1}").mkString("\n")
      for {
        stub <- stubBinary("d2-quitter", Some("#!/bin/sh\nexit 3\n"))
        doc = Doc.doc(CodeBlock.codeBlock(bigSource).language("d2"))
        renders <- D2.render(doc, stub.pathAsString)
      } yield assertTrue(
        // Larger than the OS pipe buffer, so the stdin write hits EPIPE.
        bigSource.length > 64 * 1024,
        // Soft failure carrying the stub's exit code, not a failed Task.
        leftErrorMessage(renders) == "d2 exited with code 3"
      )
    },
    test("failure keeps CodeBlock with original language (d2)")(
      failureKeepsCodeBlock("d2")
    ),
    test("failure keeps CodeBlock with original language (d2.png)")(
      failureKeepsCodeBlock("d2.png")
    ),
    test("failure keeps CodeBlock with original language (d2.svg)")(
      failureKeepsCodeBlock("d2.svg")
    ),
    test("failure sibling is a text CodeBlock under the details header") {
      for {
        doc <- Parser.parseMarkdown(shapes)
        transformed <- D2.transform(doc, missingBinary)
      } yield {
        val block = transformed
          .allNodesOfType(classOf[CodeBlock])
          .filter(_.language().orElse("").startsWith("d2"))
          .toScala(List)
          .head
        val sibling = textSiblingAfter(transformed, block).get
        assertTrue(
          sibling.language().orElse("") == "text",
          sibling.toPlainText.startsWith(D2.RenderFailureDetailsHeader)
        )
      }
    },
    test("failing d2 block nested in Panel puts sibling inside the panel") {
      val doc = Doc.doc(
        Panel.info(
          CodeBlock.codeBlock("x -> y").language("d2")
        )
      )

      for {
        transformed <- D2.transform(doc, missingBinary)
      } yield {
        val panel = transformed
          .allNodesOfType(classOf[Panel])
          .toScala(List)
          .head
        assertTrue(
          panel.allNodesOfType(classOf[CodeBlock]).count() == 2
        )
      }
    },
    test("non-d2 CodeBlocks are unchanged") {
      val markdown =
        """```scala
          |val x = 1
          |```
          |
          |```plantuml
          |Alice -> Bob: hi
          |```
          |
          |```mermaid
          |sequenceDiagram
          |    Alice->>Bob: Hello
          |```""".stripMargin

      for {
        doc <- Parser.parseMarkdown(markdown)
        transformed <- D2.transform(doc)
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
          languages.contains("mermaid"),
          externalMedia.isEmpty
        )
      }
    }
  )
}
