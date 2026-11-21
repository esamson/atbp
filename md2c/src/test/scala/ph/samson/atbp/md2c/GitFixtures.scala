package ph.samson.atbp.md2c

import better.files.File
import ph.samson.atbp.md2c.GitMetadata.Runner
import zio.Scope
import zio.Trace
import zio.ZIO
import zio.test.TestAspect
import zio.test.TestFailure
import zio.test.TestSuccess

import java.nio.charset.StandardCharsets.UTF_8

/** Isolated, offline repositories: no personal Git config or inherited
  * repository variables, explicit identity and dates, local paths only.
  */
private[md2c] object GitFixtures {

  /** Removes personal config and inherited repository variables. */
  def isolate(env: java.util.Map[String, String]): Unit = {
    ProcessRunner.RepositoryOverrides.foreach(env.remove(_): Unit)
    env.remove("GIT_CONFIG")
    env.remove("GIT_CONFIG_PARAMETERS")
    env.put("GIT_CONFIG_GLOBAL", "/dev/null")
    env.put("GIT_CONFIG_NOSYSTEM", "1")
    env.put("GIT_CONFIG_COUNT", "0"): Unit
  }

  /** The metadata runner for tests: isolated from personal Git config. */
  private[md2c] val runner: Runner = ProcessRunner.processRunner(
    "git",
    ProcessRunner.DefaultTimeoutMillis,
    ProcessRunner.DefaultMaxOutputBytes,
    ProcessRunner.DefaultConcurrency,
    isolate
  )

  /** Installs the isolated runner around each test, so tests that install their
    * own fake runner still win.
    */
  val isolated: TestAspect[Nothing, Any, Nothing, Any] =
    new TestAspect.PerTest[Nothing, Any, Nothing, Any] {
      override def perTest[R <: Any, E <: Any](
          test: ZIO[R, TestFailure[E], TestSuccess]
      )(implicit
          trace: Trace
      ): ZIO[R, TestFailure[E], TestSuccess] =
        GitMetadata.withRunner(runner)(test)
    }

  /** A fresh temporary directory, deleted when the scope closes. */
  def temp: ZIO[Scope, Throwable, File] =
    ZIO.acquireRelease(ZIO.attemptBlocking(File.newTemporaryDirectory()))(dir =>
      ZIO.attemptBlocking(dir.delete(swallowIOExceptions = true)).orDie
    )

  /** Runs git in `dir`, failing unless it exits 0; returns stdout. */
  def git(dir: File, args: String*): String = {
    val builder = new ProcessBuilder(("git" +: args)*)
      .directory(dir.toJava)
      .redirectErrorStream(true)
    val env = builder.environment()
    isolate(env)
    env.put("HOME", dir.path.toString)
    env.put("GIT_TERMINAL_PROMPT", "0")
    val process = builder.start()
    process.getOutputStream.close()
    val output = new String(process.getInputStream.readAllBytes(), UTF_8)
    val code = process.waitFor()
    require(code == 0, s"git ${args.mkString(" ")} failed ($code): $output")
    output
  }

  def init(dir: File): Unit = {
    dir.createDirectoryIfNotExists()
    run(dir, "init", "-q", "-b", "main")
    run(dir, "config", "user.name", "Fixture Author")
    run(dir, "config", "user.email", "author@example.test")
    run(dir, "config", "commit.gpgsign", "false")
  }

  /** Commit everything with a fixed author date. */
  def commitAll(dir: File, message: String, date: String): Unit = {
    run(dir, "add", "-A")
    run(dir, "commit", "-q", "--date", date, "-m", message)
  }

  /** Write a file, then commit all changes. */
  def commitFile(
      dir: File,
      path: String,
      content: String,
      message: String,
      date: String
  ): File = {
    val file = dir / path
    file.parent.createDirectoryIfNotExists(createParents = true)
    file.write(content)
    commitAll(dir, message, date)
    file
  }

  /** A repository with three commits touching `docs/guide.md` and one other. */
  def history(dir: File): Unit = {
    init(dir)
    commitFile(
      dir,
      "docs/guide.md",
      "one\n",
      "First",
      "2026-01-01T10:00:00Z"
    ): Unit
    commitFile(
      dir,
      "docs/guide.md",
      "two\n",
      "Second",
      "2026-01-02T10:00:00Z"
    ): Unit
    commitFile(
      dir,
      "docs/guide.md",
      "three\n",
      "Third",
      "2026-01-03T10:00:00Z"
    ): Unit
    commitFile(
      dir,
      "docs/other.md",
      "x\n",
      "Other",
      "2026-01-04T10:00:00Z"
    ): Unit
  }

  def clone(origin: File, target: File, extra: String*): Unit =
    git(
      target.parent,
      ("clone" +: "-q" +: extra) ++ Seq(origin.uri.toString, target.name)*
    ): Unit

  /** Run git for its effect only. */
  def run(dir: File, args: String*): Unit = git(dir, args*): Unit
}
