package ph.samson.atbp.cli

import better.files.File
import ph.samson.atbp.cli.Plate.Action
import ph.samson.atbp.jira.Client
import ph.samson.atbp.plate.Inspector
import ph.samson.atbp.plate.Labeler
import zio.ZIO
import zio.cli.Args
import zio.cli.Command
import zio.cli.Exists.Yes
import zio.cli.Options
import zio.http.ZClient

import java.time.ZoneId

case class Plate(action: Action) extends ToolCommand {
  override def run(conf: Conf): ZIO[Any, Throwable, Unit] = action.run(conf)
}

object Plate {
  import ph.samson.atbp.cli.Plate.Check.Status

  private val source = Args.file("source", Yes)

  sealed trait Action extends ToolCommand

  private case class Label(source: File, value: String) extends Action {
    override def run(conf: Conf): ZIO[Any, Throwable, Unit] = {
      conf.jiraConf match {
        case None => ZIO.fail(new Exception("No jira config."))
        case Some(jira) =>
          doRun().provide(ZClient.default, Client.layer(jira), Labeler.layer())
      }
    }

    def doRun() = ZIO.logSpan("label") {
      for {
        labeler <- ZIO.service[Labeler]
        _ <- labeler.label(source, value)
      } yield ()
    }
  }

  private object Label {
    val value = Options.text("value")
    val command = Command("label", Label.value, source).map { (v, s) =>
      Label(s, v)
    }
  }

  private case class Check(source: File, status: Status) extends Action {
    override def run(conf: Conf): ZIO[Any, Throwable, Unit] = {
      conf.jiraConf match {
        case None => ZIO.fail(new Exception("No jira config."))
        case Some(jira) =>
          val check = for {
            inspector <- ZIO.service[Inspector]
            result <- status match {
              case Check.Cooking => inspector.cooking(source)
              case Check.Stale   => inspector.stale(source)
              case Check.Done    => inspector.done(source)
            }
            _ <- ZIO.logInfo(s"check result: $result")
          } yield ()

          check.provide(ZClient.default, Client.layer(jira), Inspector.layer())
      }
    }
  }

  private object Check {
    val PHT = ZoneId.of("Asia/Manila")

    sealed trait Status
    case object Cooking extends Status
    case object Stale extends Status
    case object Done extends Status

    val status: Options[Status] = Options.enumeration("status")(
      "cooking" -> Cooking,
      "stale" -> Stale,
      "done" -> Done
    )

    val command = Command("check", status, source).map {
      case (status, source) =>
        Check(source, status)
    }
  }

  val command: Command[Plate] =
    Command("plate").subcommands(Label.command, Check.command).map(Plate.apply)
}
