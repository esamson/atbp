package ph.samson.atbp.md2c

import better.files.File
import ph.samson.atbp.md2c.GitMetadata.*
import zio.Semaphore
import zio.Task
import zio.Unsafe
import zio.ZIO

import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import scala.jdk.StreamConverters.*

object ProcessRunner {
  private[md2c] val RepositoryOverrides = List(
    "GIT_DIR",
    "GIT_WORK_TREE",
    "GIT_INDEX_FILE",
    "GIT_COMMON_DIR",
    "GIT_OBJECT_DIRECTORY",
    "GIT_ALTERNATE_OBJECT_DIRECTORIES",
    "GIT_NAMESPACE",
    "GIT_PREFIX",
    "GIT_CEILING_DIRECTORIES",
    "GIT_DISCOVERY_ACROSS_FILESYSTEM",
    "GIT_GLOB_PATHSPECS",
    "GIT_NOGLOB_PATHSPECS",
    "GIT_ICASE_PATHSPECS"
  )

  private[md2c] val DefaultTimeoutMillis = 10000L
  private[md2c] val DefaultMaxOutputBytes = 1024 * 1024
  private[md2c] val DefaultConcurrency =
    math.max(2, Runtime.getRuntime.availableProcessors())

  private def classifyDiagnostic(
      arguments: List[String],
      exitCode: Int,
      stderr: String
  ): Option[Diagnostic] =
    (arguments, stderr.linesIterator.nextOption()) match {
      case (
            List("rev-parse", "--show-toplevel"),
            Some("fatal: this operation must be run in a work tree")
          ) if exitCode == 128 =>
        Some(Diagnostic.BareRepository)
      case (List("rev-parse", "--show-toplevel"), Some(line))
          if exitCode == 128 &&
            (line == "fatal: not a git repository (or any of the parent directories): .git" ||
              line.startsWith(
                "fatal: not a git repository (or any parent up to mount point "
              )) =>
        Some(Diagnostic.RepositoryNotFound)
      case (
            List("rev-parse", "--verify", "HEAD"),
            Some("fatal: Needed a single revision")
          ) if exitCode == 128 =>
        Some(Diagnostic.UnresolvedHead)
      case _ => None
    }

  /** Adjusts the child environment after the runner's own defaults. */
  private[md2c] type EnvironmentAdjustment =
    java.util.Map[String, String] => Unit

  private[md2c] val NoAdjustment: EnvironmentAdjustment = _ => ()

  private[md2c] def processRunner(
      executable: String,
      timeoutMillis: Long,
      maxOutputBytes: Int,
      maxConcurrent: Int
  ): Runner =
    processRunner(
      executable,
      timeoutMillis,
      maxOutputBytes,
      maxConcurrent,
      NoAdjustment
    )

  private[md2c] def processRunner(
      executable: String,
      timeoutMillis: Long,
      maxOutputBytes: Int,
      maxConcurrent: Int,
      adjustEnvironment: EnvironmentAdjustment
  ): Runner =
    new ProcessRunner(
      executable,
      timeoutMillis,
      maxOutputBytes,
      maxConcurrent,
      adjustEnvironment
    )
}

/** Runs one noninteractive, bounded command and classifies known Git errors.
  */
private[md2c] class ProcessRunner(
    executable: String,
    timeoutMillis: Long,
    maxOutputBytes: Int,
    maxConcurrent: Int,
    adjustEnvironment: ProcessRunner.EnvironmentAdjustment
) extends Runner {
  private val permits =
    Unsafe.unsafe(implicit u => Semaphore.unsafe.make(maxConcurrent.toLong))

  override def run(
      directory: File,
      arguments: List[String]
  ): Task[CommandResult] =
    permits.withPermit(
      ZIO
        .attemptBlockingInterrupt(execute(directory, arguments))
        .flatMap(ZIO.fromEither(_))
    )

  private def executableAvailable: Boolean =
    if (executable.contains(java.io.File.separator))
      new java.io.File(executable).canExecute
    else
      Option(System.getenv("PATH")).exists(
        _.split(java.io.File.pathSeparatorChar).exists(dir =>
          dir.nonEmpty && new java.io.File(dir, executable).canExecute
        )
      )

  private def terminate(process: Process): Unit = {
    val children = process.descendants().toScala(List)
    process.destroy()
    children.foreach(_.destroy())
    if (!process.waitFor(200, TimeUnit.MILLISECONDS)) {
      process.destroyForcibly()
      process.waitFor(1, TimeUnit.SECONDS): Unit
    }
    children.foreach(child => if (child.isAlive) child.destroyForcibly(): Unit)
  }

  private def execute(
      directory: File,
      arguments: List[String]
  ): Either[CommandFailure, CommandResult] = {
    val captureDiagnostic =
      arguments == List("rev-parse", "--show-toplevel") ||
        arguments == List("rev-parse", "--verify", "HEAD")
    val builder =
      // A repository-local core.fsmonitor command must not run during diff.
      new ProcessBuilder(
        (executable :: "-c" :: "core.fsmonitor=false" :: arguments)*
      )
        .directory(directory.toJava)
    if (!captureDiagnostic)
      builder.redirectError(ProcessBuilder.Redirect.DISCARD): Unit
    val env = builder.environment()
    // Use the source's worktree and match its path exactly in all queries.
    ProcessRunner.RepositoryOverrides.foreach(env.remove(_): Unit)
    env.put("GIT_LITERAL_PATHSPECS", "1")
    env.put("GIT_TERMINAL_PROMPT", "0")
    env.put("GIT_PAGER", "cat")
    env.put("GIT_OPTIONAL_LOCKS", "0")
    env.put("GCM_INTERACTIVE", "never")
    env.put("GIT_NO_LAZY_FETCH", "1")
    // Older Git can attempt a lazy fetch. Allow no transport protocols.
    env.put("GIT_ALLOW_PROTOCOL", "")
    env.put("LC_ALL", "C")
    adjustEnvironment(env)
    val started =
      try Right(builder.start())
      catch {
        case _: java.io.IOException =>
          Left(
            if (!executableAvailable) new ExecutableUnavailable
            else new Failed("start-failed")
          )
      }
    started.flatMap(process => collect(process, arguments, captureDiagnostic))
  }

  private class ProcessOutput(in: InputStream, limit: Int) {
    private val output = new ByteArrayOutputStream
    val overflow = new AtomicBoolean(false)
    val reader = new Thread(() =>
      try {
        val buffer = new Array[Byte](8192)
        @scala.annotation.tailrec
        def drain(retained: Int): Unit = {
          val count = in.read(buffer)
          if (count >= 0) {
            val keep = math.min(count, limit - retained)
            if (keep > 0) output.synchronized {
              output.write(buffer, 0, keep)
            }
            if (keep < count) overflow.set(true)
            drain(retained + math.max(0, keep))
          }
        }
        drain(0)
      } catch { case _: java.io.IOException => () }
    )
    reader.setDaemon(true)

    def value: String =
      output.synchronized(new String(output.toByteArray, UTF_8))

    def close(): Unit = {
      try in.close()
      catch { case _: java.io.IOException => () }
      reader.interrupt()
    }
  }

  private def collect(
      process: Process,
      arguments: List[String],
      captureDiagnostic: Boolean
  ): Either[CommandFailure, CommandResult] = {
    val output = new ProcessOutput(process.getInputStream, maxOutputBytes)
    val diagnostic = Option.when(captureDiagnostic)(
      new ProcessOutput(
        process.getErrorStream,
        math.min(maxOutputBytes, 4096)
      )
    )
    val streams = output :: diagnostic.toList
    val deadline =
      System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(timeoutMillis)
    try {
      process.getOutputStream.close()
      streams.foreach(_.reader.start())
      if (!process.waitFor(deadline - System.nanoTime(), TimeUnit.NANOSECONDS))
        Left(new TimedOut)
      else {
        streams.foreach { stream =>
          val remaining = deadline - System.nanoTime()
          if (remaining > 0)
            TimeUnit.NANOSECONDS.timedJoin(stream.reader, remaining)
        }
        if (streams.exists(_.reader.isAlive)) Left(new TimedOut)
        else if (output.overflow.get()) Left(new Failed("output-limit"))
        else {
          val exitCode = process.exitValue()
          Right(
            CommandResult(
              exitCode,
              output.value,
              diagnostic
                .filterNot(_.overflow.get())
                .flatMap(stream =>
                  ProcessRunner
                    .classifyDiagnostic(arguments, exitCode, stream.value)
                )
            )
          )
        }
      }
    } finally {
      if (process.isAlive) terminate(process)
      streams.foreach(_.close())
    }
  }
}
