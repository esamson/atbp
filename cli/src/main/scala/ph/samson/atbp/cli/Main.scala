package ph.samson.atbp.cli

import zio.Cause
import zio.Console
import zio.ExitCode
import zio.Runtime
import zio.ZIO
import zio.ZIOAppArgs
import zio.ZLayer
import zio.cli.CliApp
import zio.cli.CliError
import zio.cli.Command
import zio.cli.HelpDoc.*
import zio.cli.HelpDoc.Span.*
import zio.cli.Options
import zio.cli.ZIOCliDefault
import zio.config.typesafe.TypesafeConfigProvider
import zio.logging.consoleErrLogger

object Main extends ZIOCliDefault {

  val Name = "atbp"

  def versionString(
      nativeVersion: Option[String],
      implementationVersion: Option[String]
  ): String =
    nativeVersion
      .filter(_.nonEmpty)
      .orElse(implementationVersion.filter(_.nonEmpty))
      .map(v => s"v$v")
      .getOrElse("(dev)")

  val Version = versionString(
    sys.props.get("ph.samson.atbp.version"),
    Option(Main.getClass.getPackage.getImplementationVersion)
  )

  /** Native images do not set java.home. AWT throws if it is missing, and
    * PlantUML needs it before measuring text. /usr is enough for the system
    * fontconfig on Linux; the image does not ship a JDK.
    */
  def ensureJavaHome(): Unit =
    if (sys.props.get("java.home").forall(_.isEmpty)) {
      val _ = System.setProperty(
        "java.home",
        sys.env.get("JAVA_HOME").filter(_.nonEmpty).getOrElse("/usr")
      )
    }

  val verbose = Options.boolean("verbose").alias("v") ?? "Print out debug logs."
  val quiet = Options.boolean("quiet").alias("q") ??
    "Only print warning logs. Takes precedence over verbose flag."
  val logging = verbose ++ quiet

  val atbp = Command(Name, logging)
    .subcommands(
      Hubad.command,
      Liga.command,
      Markdown2Confluence.command,
      MarkdownMerge.command,
      Plate.command,
      Retext.command,
      Stmt2Csv.command,
      TraceViz.command
    )
    .withHelp(
      blocks(
        p(
          "Unrelated tools that I'm too lazy to build and package and install" +
            " separately. Each tool is its own command. See list below."
        ),
        p(
          spans(
            text("Use "),
            code("atbp <command> --help"),
            text(" for specific help for each tool.")
          )
        ),
        h2("Configuration"),
        p(
          spans(
            text("User configuration is read from "),
            code(Conf.Resolver.get("application.conf").pathAsString),
            text(" as a HOCON formatted file.")
          )
        )
      )
    )

  override def run = {
    ensureJavaHome()
    for {
      args <- ZIOAppArgs.getArgs
      result <-
        if (args == zio.Chunk.single("--version")) {
          Console.printLine(Version).as(ExitCode.success)
        } else {
          cliApp.run(args.toList).catchSome { case CliError.Parsing(_) =>
            exit(ExitCode.failure)
          }
        }
    } yield result
  }

  override def cliApp = CliApp.make(
    name = Name,
    version = Version,
    summary = text("Assorted tooling bits and pieces"),
    command = atbp
  ) { case ((verbose, quiet), toolCommand) =>
    val run = for {
      conf <- Conf.appConf
      _ <- toolCommand.run(conf)
    } yield ()

    val logger = if (quiet) {
      quietLogger
    } else if (verbose) {
      verboseLogger
    } else {
      regularLogger
    }

    val exitHandler = run.catchSomeCause { case f @ Cause.Fail(throwable, _) =>
      throwable match {
        case _: IllegalArgumentException => logAndExit(f, ExitCode(1))
        case _                           =>
          for {
            _ <- ZIO.logErrorCause(s"Unhandled $throwable", f)
            u <- logAndExit(f, ExitCode(11))
          } yield u
      }
    }

    exitHandler.provide(logger)
  }

  def logAndExit(
      cause: Cause.Fail[Throwable],
      exitCode: ExitCode
  ): ZIO[Any, Nothing, Unit] =
    for {
      _ <- ZIO.logDebugCause(cause)
      _ <- ZIO.logError(cause.value.getMessage)
      _ <- exit(exitCode)
    } yield ()

  lazy val quietLogger = {
    val configProvider = TypesafeConfigProvider.fromHoconString(
      """logger {
        |  format = "%message%cause"
        |  filter {
        |    rootLevel = WARN
        |  }
        |}
        |""".stripMargin
    )
    Runtime.removeDefaultLoggers
      >>> Runtime.setConfigProvider(configProvider)
      >>> consoleErrLogger()
  }

  lazy val regularLogger = {
    val configProvider = TypesafeConfigProvider.fromHoconString(
      """logger {
        |  format = "%message%cause"
        |  filter {
        |    rootLevel = INFO
        |  }
        |}
        |""".stripMargin
    )
    Runtime.removeDefaultLoggers
      >>> Runtime.setConfigProvider(configProvider)
      >>> consoleErrLogger()
  }

  lazy val verboseLogger = {
    val configProvider = TypesafeConfigProvider.fromHoconString(
      """logger {
        |  format = "[%level %name][%spans][%kvs] %message%cause"
        |  filter {
        |    rootLevel = DEBUG
        |  }
        |}
        |""".stripMargin
    )
    Runtime.removeDefaultLoggers
      >>> Runtime.setConfigProvider(configProvider)
      >>> consoleErrLogger()
  }
}
