package ph.samson.atbp.md2c

import better.files.File
import com.aresstack.Mermaid as JavaMermaid
import com.aresstack.mermaid.JsExecutionResult
import com.atlassian.adf.model.node.CodeBlock
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Media.externalMedia
import com.atlassian.adf.model.node.MediaSingle.mediaSingle
import com.atlassian.adf.model.node.Node
import com.atlassian.adf.model.node.RichMedia
import com.atlassian.adf.model.node.Text
import com.atlassian.adf.model.node.`type`.DocContent
import zio.Task
import zio.ZIO

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import javax.imageio.ImageIO
import scala.jdk.CollectionConverters.*
import scala.jdk.FunctionConverters.*
import scala.jdk.StreamConverters.*

object Mermaid {

  val RenderFailureComment =
    "%% Mermaid rendering failed. See details below."

  val RenderFailureDetailsHeader = "Mermaid failure details:"

  val DefaultFormat = "png"

  type RenderOutcome = Either[JsExecutionResult, File]

  /** `(source, format)` so identical bodies with different fences do not
    * collide.
    */
  type RenderKey = (String, String)

  private def formatFromLanguage(language: String): String =
    if (language.contains(".svg")) "svg"
    else if (language.contains(".png")) "png"
    else DefaultFormat

  private def isMermaid(codeBlock: CodeBlock): Boolean =
    codeBlock.language().orElse("").startsWith("mermaid")

  private def renderKey(codeBlock: CodeBlock): RenderKey = {
    val source = codeBlock.toPlainText
    val format =
      codeBlock.language().map(formatFromLanguage(_)).orElse(DefaultFormat)
    (source, format)
  }

  /** Same soft-failure signals as {@code MermaidRenderer.renderToSvg}. */
  private def classifyJsResult(
      result: JsExecutionResult
  ): Either[JsExecutionResult, String] =
    if (!result.isSuccessful) {
      Left(result)
    } else {
      Option(result.getOutput()).filter(_.nonEmpty) match {
        case None =>
          Left(JsExecutionResult.failure("Empty mermaid output"))
        case Some(output) if output.startsWith("ERROR:") =>
          Left(JsExecutionResult.failure(output.substring("ERROR:".length)))
        case Some(output) if !output.contains("<svg") =>
          Left(
            JsExecutionResult.failure(
              s"Output does not contain <svg>: ${output.take(500)}"
            )
          )
        case Some(output) =>
          Right(output)
      }
    }

  private def insertAfterParentIndex(
      parent: Node,
      index: Int,
      node: CodeBlock
  ): Unit = {
    parent.getClass.getMethod("content").invoke(parent) match {
      case content: java.util.List[?] @unchecked =>
        val updated = new java.util.ArrayList[AnyRef](content)
        updated.add(index + 1, node)
        parent.getClass
          .getMethod("replaceContent", classOf[java.util.List[?]])
          .invoke(parent, updated)
        ()
      case _ =>
        ()
    }
  }

  private def insertFailureSiblings(
      adf: Doc,
      errorByBlock: Map[CodeBlock, String]
  ): Unit =
    if (errorByBlock.nonEmpty) {
      val failedBlocks = errorByBlock.keySet
      val predicate = (codeBlock: CodeBlock) => failedBlocks.contains(codeBlock)
      val insertions = adf
        .findMatchingDescendants(classOf[CodeBlock], predicate.asJavaPredicate)
        .asScala
        .toList
        .flatMap { container =>
          container.children().asScala.toList.map { childMatch =>
            (container.parent(), childMatch.index(), childMatch.`match`())
          }
        }
        .sortBy(-_._2)

      insertions.foreach { (parent, index, codeBlock) =>
        val errorMessage = errorByBlock(codeBlock)
        val errorBlock = CodeBlock
          .codeBlock(s"$RenderFailureDetailsHeader\n$errorMessage")
          .language("text")
        insertAfterParentIndex(parent, index, errorBlock)
      }
    }

  def render(adf: Doc): ZIO[Any, Throwable, Map[RenderKey, RenderOutcome]] = {
    def renders(outDir: File) = adf
      .allNodesOfType(classOf[CodeBlock])
      .filter(isMermaid(_))
      .toScala(List)
      .zipWithIndex
      .map { (codeBlock, index) =>
        val (source, format) = renderKey(codeBlock)
        for {
          outcome <- ZIO
            .attemptBlocking {
              // renderDetailed returns raw SVG without postProcessSvg
              // (xhtml xmlns strip, width=100%→px, …). Batik then paints
              // blank and autoCrop collapses to 8×8. Use render() for the
              // success path (postProcess + fixForBatik); fall back to
              // renderDetailed only for the soft-failure error message.
              Option(JavaMermaid.render(source)) match {
                case Some(fixed) =>
                  format match {
                    case "svg" =>
                      val outFile =
                        outDir / s"fig-${index + 1}.mermaid.$format"
                      outFile.writeByteArray(fixed.getBytes(UTF_8))
                      Some(Right(outFile): RenderOutcome)
                    case _ =>
                      Option(JavaMermaid.svgToImage(fixed)).map { image =>
                        val outFile =
                          outDir / s"fig-${index + 1}.mermaid.$format"
                        val os = new ByteArrayOutputStream()
                        ImageIO.write(image, "png", os)
                        outFile.writeByteArray(os.toByteArray)
                        Right(outFile): RenderOutcome
                      }
                  }
                case None =>
                  classifyJsResult(JavaMermaid.renderDetailed(source)) match {
                    case Left(failure) =>
                      Some(Left(failure): RenderOutcome)
                    case Right(_) =>
                      None
                  }
              }
            }
            .flatMap {
              case Some(value) => ZIO.succeed(value)
              case None        =>
                ZIO.fail(
                  new RuntimeException(
                    "Mermaid.render/svgToImage returned null without a soft-failure detail"
                  )
                )
            }
        } yield (source, format) -> outcome
      }

    for {
      outDir <- ZIO.attemptBlocking(File.newTemporaryDirectory())
      rendered <- ZIO.collectAllPar(renders(outDir)).map(_.toMap)
    } yield rendered
  }

  def transform(adf: Doc): Task[Doc] = {
    def transformer(
        renders: Map[RenderKey, RenderOutcome]
    ): DocContent => DocContent = {
      case codeBlock: CodeBlock =>
        if (isMermaid(codeBlock)) {
          val (source, format) = renderKey(codeBlock)
          renders((source, format)) match {
            case Right(file) =>
              mediaSingle(
                RichMedia.Layout.FULL_WIDTH,
                externalMedia(file.pathAsString)
              )
            case Left(_) =>
              codeBlock.replaceContent(
                java.util.List.of(
                  Text.text(s"$RenderFailureComment\n$source")
                )
              )
              codeBlock
          }
        } else {
          codeBlock
        }
      case other => other
    }

    for {
      renders <- render(adf)
      mermaidBlocks = adf
        .allNodesOfType(classOf[CodeBlock])
        .filter(isMermaid(_))
        .toScala(List)
      errorByBlock = mermaidBlocks.flatMap { codeBlock =>
        renders(renderKey(codeBlock)).left.toOption.map { result =>
          codeBlock -> result.getErrorMessage()
        }
      }.toMap
      xfrm = transformer(renders).asJavaFunction
      _ = adf.transformDescendants(classOf[DocContent], xfrm)
      _ = insertFailureSiblings(adf, errorByBlock)
    } yield {
      adf
    }
  }
}
