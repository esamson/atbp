package ph.samson.atbp.cli

import better.files.File
import ph.samson.atbp.c2md.Converter
import ph.samson.atbp.confluence.Client
import zio.Console
import zio.ZIO
import zio.cli.Args
import zio.cli.Command
import zio.cli.Exists.Either
import zio.cli.HelpDoc.*
import zio.cli.Options
import zio.http.ZClient

case class Confluence2Markdown(pageId: String, target: Option[File])
    extends ToolCommand {
  override def run(conf: Conf): ZIO[Any, Throwable, Unit] =
    conf.confluenceConf match {
      case None             => ZIO.fail(new Exception("No confluence config."))
      case Some(confluence) =>
        doRun.provide(
          ZClient.default,
          Client.layer(confluence),
          Converter.layer
        )
    }

  private def doRun = for {
    converter <- ZIO.service[Converter]
    markdown <- converter.convert(pageId)
    _ <- target match {
      case Some(file) => ZIO.attemptBlockingIO(file.overwrite(markdown))
      case None       => Console.printLine(markdown)
    }
  } yield ()
}

object Confluence2Markdown {
  private val target = Options.file("target", Either).optional
  private val pageId = Args.text("pageId")

  val command: Command[Confluence2Markdown] =
    Command("c2md", target, pageId)
      .withHelp(
        blocks(
          h2("Confluence to Markdown"),
          p("Download a Confluence page by ID and convert its body to Markdown.")
        )
      )
      .map { case (target, id) =>
        Confluence2Markdown(
          pageId = id,
          target = target.map(File.apply)
        )
      }
}
