package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.model.node.CodeBlock
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Media.externalMedia
import com.atlassian.adf.model.node.MediaSingle.mediaSingle
import com.atlassian.adf.model.node.RichMedia
import com.atlassian.adf.model.node.Text
import com.atlassian.adf.model.node.`type`.DocContent
import zio.Semaphore
import zio.Task
import zio.UIO
import zio.Unsafe
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

  /** d2 renders in its own OS process, so unlike md2c's in-JVM renderers each
    * concurrent fence costs a whole process image rather than a thread. The cap
    * is a single module-level semaphore shared by the whole run, not
    * `withParallelism`: `StagedTree` converts pages in parallel, so a per-page
    * bound would still leave the tree-wide process count unbounded.
    */
  private val MaxConcurrentRenders =
    math.max(2, java.lang.Runtime.getRuntime().availableProcessors())

  private val renderPermits: Semaphore =
    Unsafe.unsafe(implicit u => Semaphore.unsafe.make(MaxConcurrentRenders))

  /** `(source, format)` so identical bodies with different fences do not
    * collide.
    */
  type RenderKey = (String, String)

  type RenderOutcome = Either[String, File]

  /** Whether `executable` resolves to something that exists: a name carrying a
    * path separator is checked directly, a bare name is looked up across
    * `PATH`. `ProcessBuilder.start()` reports a missing binary, a file without
    * the exec bit and a failed `fork` all as a plain `IOException`, so this is
    * the only way to tell "not installed" apart from "installed but unusable"
    * without sniffing JDK- and platform-specific exception messages.
    */
  private def onPath(executable: String): Boolean =
    if (executable.contains(java.io.File.separator)) {
      File(executable).exists
    } else {
      Option(System.getenv("PATH")).exists(
        _.split(java.io.File.pathSeparatorChar)
          .exists(dir => dir.nonEmpty && (File(dir) / executable).exists)
      )
    }

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
    * `IOException` from `ProcessBuilder.start()`, and so does every other way
    * starting `d2` can fail (no exec bit, `fork` refused under load). Both are
    * soft failures, but only a binary that is genuinely absent from `PATH` gets
    * `NotOnPathMessage` — anything else reports the OS error, so a broken
    * install is not misdiagnosed as a missing one. Exceptions that are not
    * `IOException` still propagate and fail the `Task`, matching Mermaid's
    * "thrown exceptions are not caught" rule.
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
      .catchSome { case e: IOException =>
        ZIO.succeed(
          Left(
            if (onPath(executable)) s"Could not render diagram: ${e.getMessage}"
            else NotOnPathMessage
          )
        )
      }

  private def runD2(
      process: Process,
      source: String,
      outFile: File,
      errFile: File
  ): RenderOutcome =
    try {
      try {
        val stdin = process.getOutputStream()
        stdin.write(source.getBytes(UTF_8))
        stdin.close()
      } catch {
        // A `d2` that exits without draining stdin breaks the pipe partway
        // through a source larger than the OS pipe buffer (~64KB). That is a
        // symptom, not the diagnosis: fall through to the exit code and
        // stderr below, which carry d2's own account of why it quit. Failing
        // the Task here would abort the whole md2c run over one diagram.
        case _: IOException => ()
      }

      if (!process.waitFor(30, TimeUnit.SECONDS)) {
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
    } finally {
      // Covers the timeout branch and any throw above: never leave a d2
      // process running with its stdin held open.
      if (process.isAlive) {
        process.destroyForcibly(): Unit
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
      .map(renderKey)
      // One subprocess per distinct key. Duplicate fences used to race for the
      // same map entry, so whichever render happened to finish last won — and
      // a transient loser (a 30s timeout, a refused fork) would overwrite a
      // perfectly good sibling render, failing both copies of the diagram.
      .distinct
      .zipWithIndex
      .map { case ((source, format), index) =>
        val outFile = outDir / s"fig-${index + 1}.d2.$format"
        val errFile = outDir / s"fig-${index + 1}.d2.$format.stderr"
        renderPermits.withPermit(
          for {
            started <- startProcess(executable, format, outFile, errFile)
            outcome <- started match {
              case Left(message) =>
                ZIO.succeed(Left(message): RenderOutcome)
              case Right(process) =>
                ZIO.attemptBlocking(runD2(process, source, outFile, errFile))
            }
          } yield (source, format) -> outcome
        )
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
