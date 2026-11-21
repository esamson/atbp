package ph.samson.atbp.md2c

import better.files.File
import zio.ZIO

/** Shared fixtures for the split GitMetadata specs. */
private[md2c] object GitSpecSupport {
  private[md2c] def withTemp[R, E, A](use: File => ZIO[R, E, A]) =
    ZIO
      .acquireRelease(ZIO.attemptBlocking(File.newTemporaryDirectory()))(dir =>
        ZIO.attemptBlocking(dir.delete(swallowIOExceptions = true)).orDie
      )
      .flatMap(use)

  /** A real source file below a temporary directory used as the fake root. */
  private[md2c] def withSource[R, E, A](use: (File, File) => ZIO[R, E, A]) =
    withTemp { dir =>
      val source = dir / "guide.md"
      ZIO.attemptBlocking(source.write("hello\n")) *> use(dir, source)
    }

  private[md2c] def withScripts[R, E, A](use: File => ZIO[R, E, A]) = withTemp(
    use
  )

  private[md2c] def configured(
      path: String,
      timeoutMillis: Long,
      maxOutputBytes: Int,
      maxConcurrent: Int
  ) = ProcessRunner.processRunner(
    path,
    timeoutMillis,
    maxOutputBytes,
    maxConcurrent,
    GitFixtures.isolate
  )

  private[md2c] def processRunner(path: String) = configured(
    path,
    ProcessRunner.DefaultTimeoutMillis,
    ProcessRunner.DefaultMaxOutputBytes,
    ProcessRunner.DefaultConcurrency
  )

  private[md2c] def timed(path: String, timeoutMillis: Long) = configured(
    path,
    timeoutMillis,
    ProcessRunner.DefaultMaxOutputBytes,
    ProcessRunner.DefaultConcurrency
  )

  private[md2c] def executable(file: File, content: String): File = {
    file.write(content)
    file.toJava.setExecutable(true)
    file
  }

  private[md2c] def poll(check: => Boolean): ZIO[Any, Throwable, Boolean] = {
    lazy val loop: ZIO[Any, Throwable, Boolean] =
      ZIO.attemptBlocking(check).flatMap { done =>
        if (done) ZIO.succeed(true)
        else ZIO.sleep(zio.Duration.fromMillis(50)) *> loop
      }
    loop
      .timeout(zio.Duration.fromSeconds(5))
      .map(_.contains(true))
  }

  private[md2c] def awaitFile(file: File) =
    poll(file.exists && file.contentAsString.trim.nonEmpty).flatMap(started =>
      ZIO.when(!started)(ZIO.fail(new RuntimeException("script not started")))
    )

  private[md2c] def processDead(pidFile: File) =
    awaitFile(pidFile) *> {
      val pid = pidFile.contentAsString.trim.toLong
      poll(!ProcessHandle.of(pid).map(_.isAlive).orElse(false))
    }
}
