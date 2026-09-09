package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.model.node.CodeBlock
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Media.externalMedia
import com.atlassian.adf.model.node.MediaSingle.mediaSingle
import com.atlassian.adf.model.node.RichMedia
import com.atlassian.adf.model.node.Text
import com.atlassian.adf.model.node.`type`.DocContent
import zio.Task
import zio.UIO
import zio.ZIO

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.TimeUnit
import scala.jdk.FunctionConverters.*
import scala.jdk.StreamConverters.*

object D2 {

  val RenderFailureComment = "# D2 rendering failed. See details below."

  val RenderFailureDetailsHeader = "D2 failure details:"

  val NotOnPathMessage = "Could not render diagram: d2 not on PATH"

  val DefaultFormat = "png"

  /** `(source, format)` so identical bodies with different fences do not
    * collide.
    */
  type RenderKey = (String, String)

  type RenderOutcome = Either[String, File]

  private def isD2(codeBlock: CodeBlock): Boolean =
    codeBlock.language().orElse("").startsWith("d2")

  private def formatFromLanguage(language: String): String =
    if (language.contains(".svg")) "svg"
    else if (language.contains(".png")) "png"
    else DefaultFormat

  private def renderKey(codeBlock: CodeBlock): RenderKey = {
    val source = codeBlock.toPlainText
    val format =
      codeBlock.language().map(formatFromLanguage(_)).orElse(DefaultFormat)
    (source, format)
  }

  /** Probes whether `d2` is actually runnable, e.g. to guard tests that need a
    * real binary. Never fails.
    */
  val d2Available: UIO[Boolean] =
    ZIO
      .attemptBlocking {
        val process = new ProcessBuilder("d2", "--version").start()
        process.getOutputStream().close()
        process.waitFor(30, TimeUnit.SECONDS) && process.exitValue() == 0
      }
      .orElseSucceed(false)

  /** Starts the `d2` subprocess. A missing binary surfaces here as
    * `IOException` from `ProcessBuilder.start()` — that, and only that, is
    * caught and mapped to `NotOnPathMessage`; every other exception (stdin
    * write, waitFor, reading the redirected files) propagates and fails the
    * `Task`, matching Mermaid's "thrown exceptions are not caught" rule.
    */
  private def startProcess(
      executable: String,
      format: String,
      outFile: File,
      errFile: File
  ): ZIO[Any, Throwable, Either[String, Process]] =
    ZIO
      .attemptBlocking {
        new ProcessBuilder(executable, "--stdout-format", format, "-", "-")
          .redirectOutput(outFile.toJava)
          .redirectError(errFile.toJava)
          .start()
      }
      .map(Right(_))
      .catchSome { case _: IOException =>
        ZIO.succeed(Left(NotOnPathMessage))
      }

  private def runD2(
      process: Process,
      source: String,
      outFile: File,
      errFile: File
  ): RenderOutcome = {
    process.getOutputStream().write(source.getBytes(UTF_8))
    process.getOutputStream().close()

    if (!process.waitFor(30, TimeUnit.SECONDS)) {
      process.destroyForcibly()
      Left("Could not render diagram: d2 timed out after 30s")
    } else {
      val exitCode = process.exitValue()
      // Success is decided by exit code alone, never by stderr content: d2
      // writes a "success: ..." line to stderr on every successful run, so
      // treating any stderr as failure would misclassify healthy renders.
      if (exitCode == 0 && outFile.nonEmpty) {
        Right(outFile)
      } else {
        val details = errFile.contentAsString.trim
        Left(
          if (details.nonEmpty) details else s"d2 exited with code $exitCode"
        )
      }
    }
  }

  private[md2c] def render(
      adf: Doc
  ): ZIO[Any, Throwable, Map[RenderKey, RenderOutcome]] = render(adf, "d2")

  private[md2c] def render(
      adf: Doc,
      executable: String
  ): ZIO[Any, Throwable, Map[RenderKey, RenderOutcome]] = {
    def renders(outDir: File) = adf
      .allNodesOfType(classOf[CodeBlock])
      .filter(isD2(_))
      .toScala(List)
      .zipWithIndex
      .map { (codeBlock, index) =>
        val (source, format) = renderKey(codeBlock)
        val outFile = outDir / s"fig-${index + 1}.d2.$format"
        val errFile = outDir / s"fig-${index + 1}.d2.$format.stderr"
        for {
          started <- startProcess(executable, format, outFile, errFile)
          outcome <- started match {
            case Left(message) =>
              ZIO.succeed(Left(message): RenderOutcome)
            case Right(process) =>
              ZIO.attemptBlocking(runD2(process, source, outFile, errFile))
          }
        } yield (source, format) -> outcome
      }

    for {
      outDir <- ZIO.attemptBlocking(File.newTemporaryDirectory())
      rendered <- ZIO.collectAllPar(renders(outDir)).map(_.toMap)
    } yield rendered
  }

  def transform(adf: Doc): Task[Doc] = transform(adf, "d2")

  private[md2c] def transform(adf: Doc, executable: String): Task[Doc] = {
    def transformer(
        renders: Map[RenderKey, RenderOutcome]
    ): DocContent => DocContent = {
      case codeBlock: CodeBlock =>
        if (isD2(codeBlock)) {
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
      renders <- render(adf, executable)
      d2Blocks = adf
        .allNodesOfType(classOf[CodeBlock])
        .filter(isD2(_))
        .toScala(List)
      errorByBlock = d2Blocks.flatMap { codeBlock =>
        renders(renderKey(codeBlock)).left.toOption.map { message =>
          codeBlock -> message
        }
      }.toMap
      xfrm = transformer(renders).asJavaFunction
      _ = adf.transformDescendants(classOf[DocContent], xfrm)
      _ = DiagramFailure.insertFailureSiblings(
        adf,
        errorByBlock,
        RenderFailureDetailsHeader
      )
    } yield {
      adf
    }
  }
}
