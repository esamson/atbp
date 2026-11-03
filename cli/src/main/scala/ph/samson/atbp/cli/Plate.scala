package ph.samson.atbp.cli

import better.files.File
import ph.samson.atbp.cli.Plate.Action
import ph.samson.atbp.jira.Client
import ph.samson.atbp.plate.CookingExclude
import ph.samson.atbp.plate.Inspector
import ph.samson.atbp.plate.Labeler
import ph.samson.atbp.plate.RadarScanner
import ph.samson.atbp.plate.Sorter
import zio.ZIO
import zio.cli.Args
import zio.cli.Command
import zio.cli.Exists.Either
import zio.cli.Exists.Yes
import zio.cli.HelpDoc.blocks
import zio.cli.HelpDoc.h2
import zio.cli.HelpDoc.p
import zio.cli.Options
import zio.http.ZClient

case class Plate(action: Action, parallel: Boolean) extends ToolCommand {
  override def run(conf: Conf): ZIO[Any, Throwable, Unit] =
    action.run(conf).withParallelism(Plate.jiraParallelism(parallel))
}

object Plate {
  import ph.samson.atbp.cli.Plate.Check.Status

  private val source = Args.file("source", Yes)
  private val target = Options.file("target", Either).optional

  sealed trait Action extends ToolCommand

  private case class Label(
      sources: List[File],
      value: String,
      exclude: List[String]
  ) extends Action {
    override def run(conf: Conf): ZIO[Any, Throwable, Unit] = {
      conf.jiraConf match {
        case None       => ZIO.fail(new Exception("No jira config."))
        case Some(jira) =>
          doRun().provide(ZClient.default, Client.layer(jira), Labeler.layer())
      }
    }

    def doRun() = ZIO.logSpan("label") {
      for {
        labeler <- ZIO.service[Labeler]
        _ <- labeler.label(sources, value, exclude)
      } yield ()
    }
  }

  private object Label {

    /** Extensions matched when expanding a directory into sources for `label`.
      */
    private val TextExtensions = Set("md", "markdown")

    private def isTextFile(file: File): Boolean =
      file.isRegularFile && file.nonEmpty &&
        file
          .extension(includeDot = false)
          .exists(ext => TextExtensions.contains(ext.toLowerCase))

    /** Files are kept as-is; directories are replaced by all text files under
      * them (recursive).
      */
    private def expandSources(sources: List[File]): List[File] =
      sources
        .flatMap {
          case dir if dir.isDirectory =>
            dir.walk().filter(isTextFile).toList.sortBy(_.pathAsString)
          case file => List(file)
        }
        .distinctBy(_.path)

    val value = Options.text("value")
    val exclude = Options
      .text("exclude")
      .map(_.split(',').toList.map(_.trim))
      .withDefault(Nil) ?? "Projects to exclude"
    val sources = Args.path("sources").atLeast(1)

    val command = Command("label", value ++ exclude, sources).map {
      case ((v, e), s) =>
        Label(expandSources(s.map(File.apply)), v, e)
    }
  }

  private case class Check(
      source: File,
      target: Option[File],
      status: Status,
      exclude: List[String]
  ) extends Action {
    override def run(conf: Conf): ZIO[Any, Throwable, Unit] = {
      conf.jiraConf match {
        case None       => ZIO.fail(new Exception("No jira config."))
        case Some(jira) =>
          val statusName = status match {
            case Check.Cooking => "cooking"
            case Check.Stale   => "stale"
            case Check.Done    => "done"
          }
          val check = for {
            _ <- ZIO
              .fromOption(CookingExclude.rejectedStatus(statusName, exclude))
              .flip
              .mapError(msg => new Exception(msg))
            inspector <- ZIO.service[Inspector]
            result <- status match {
              case Check.Cooking => inspector.cooking(source, target, exclude)
              case Check.Stale   => inspector.stale(source, target)
              case Check.Done    => inspector.done(source, target)
            }
            _ <- ZIO.logInfo(s"check result: $result")
          } yield ()

          check.provide(ZClient.default, Client.layer(jira), Inspector.layer())
      }
    }
  }

  private object Check {
    sealed trait Status
    case object Cooking extends Status
    case object Stale extends Status
    case object Done extends Status

    val status: Options[Status] = Options.enumeration("status")(
      "cooking" -> Cooking,
      "stale" -> Stale,
      "done" -> Done
    )

    val exclude = Options
      .text("exclude")
      .map(_.split(',').toList.map(_.trim).filter(_.nonEmpty))
      .withDefault(Nil) ?? "Projects to exclude"

    val command = Command("check", status ++ target ++ exclude, source).map {
      case ((status, target, exclude), source) =>
        Check(source, target.map(File(_)), status, exclude)
    }
  }

  private case class Radar(
      source: File,
      target: Option[File],
      exclude: List[String]
  ) extends Action {
    override def run(conf: Conf): ZIO[Any, Throwable, Unit] = {
      conf.jiraConf match {
        case None       => ZIO.fail(new Exception("No jira config."))
        case Some(jira) =>
          val scan = for {
            scanner <- ZIO.service[RadarScanner]
            result <- scanner.scan(source, target, exclude)
            _ <- ZIO.logInfo(s"radar result: $result")
          } yield ()

          scan.provide(
            ZClient.default,
            Client.layer(jira),
            RadarScanner.layer()
          )
      }
    }
  }

  private object Radar {
    val exclude = Options
      .text("exclude")
      .map(_.split(',').toList.map(_.trim))
      .withDefault(Nil) ?? "Projects to exclude"

    val command = Command("radar", target ++ exclude, source).map {
      case ((target, exclude), source) =>
        Radar(source, target.map(File(_)), exclude)
    }
  }

  private case class Sort(source: File) extends Action {
    override def run(conf: Conf): ZIO[Any, Throwable, Unit] = {
      conf.jiraConf match {
        case None       => ZIO.fail(new Exception("No jira config."))
        case Some(jira) =>
          doRun().provide(ZClient.default, Client.layer(jira), Sorter.layer())
      }
    }

    def doRun() = ZIO.logSpan("sort") {
      for {
        sorter <- ZIO.service[Sorter]
        result <- sorter.sort(source)
        _ <- ZIO.logInfo(s"sorted: $result")
      } yield ()
    }
  }

  private object Sort {
    val command = Command("sort", source)
      .map(s => Sort(s))
      .withHelp("Sort Jira tickets according to their order in the Plate")
  }

  val SerialParallelism: Int = 1
  val FullParallelism: Int = 100

  def jiraParallelism(parallel: Boolean): Int =
    if (parallel) FullParallelism else SerialParallelism

  private val parallel = Options
    .boolean("parallel")
    .withDefault(false) ??
    "Run Jira calls at full parallelism (up to 100). Default is one at a time."

  val command: Command[Plate] =
    Command("plate", parallel)
      .withHelp(
        blocks(
          h2("Plate Tools"),
          p("Tools for managing The Plate."),
          p(
            "Jira calls run one at a time. Pass --parallel before the " +
              "subcommand to allow up to 100 at once."
          )
        )
      )
      .subcommands(Label.command, Check.command, Radar.command, Sort.command)
      .map { case (parallel, action) =>
        Plate(action, parallel)
      }
}
