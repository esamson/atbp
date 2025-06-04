package ph.samson.atbp.cli

import zio.Cause
import zio.ExitCode
import zio.Runtime
import zio.ZIO
import zio.ZLayer
import zio.cli.CliApp
import zio.cli.Command
import zio.cli.HelpDoc.*
import zio.cli.HelpDoc.Span.*
import zio.cli.Options
import zio.cli.ZIOCliDefault
import zio.config.typesafe.TypesafeConfigProvider
import zio.logging.consoleErrLogger

object Main extends ZIOCliDefault {

  val Name = "atbp"

  val Version = {
    // read version info from JAR manifest
    val pak = Main.getClass.getPackage
    Option(pak.getImplementationVersion) match {
      case Some(version) => s"v$version"
      case None          =>
        // we're running unpackaged
        "(dev)"
    }
  }

  val verbose = Options.boolean("verbose").alias("v") ?? "Print out debug logs."
  val quiet = Options.boolean("quiet").alias("q") ??
    "Only print warning logs. Takes precedence over verbose flag."
  val logging = verbose ++ quiet

  val atbp = Command(Name, logging)
    .subcommands(
      Markdown2Confluence.command,
      Plate.command,
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
      }
    }

    exitHandler.provide(logger)
  }

  def logAndExit(cause: Cause.Fail[Throwable], exitCode: ExitCode) =
    for {
      - <- ZIO.logDebugCause(cause)
      _ <- ZIO.logError(cause.value.getMessage)
      _ <- exit(exitCode)
    } yield ()

  lazy val quietLogger = {
    val configProvider = TypesafeConfigProvider.fromHoconString(
      """logger {
        |  format = "%message"
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
        |  format = "%message"
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
