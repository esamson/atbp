package ph.samson.atbp.cli

import better.files.File
import ph.samson.atbp.confluence.Client
import ph.samson.atbp.md2c
import ph.samson.atbp.md2c.OfflineRender
import ph.samson.atbp.md2c.Publisher
import ph.samson.atbp.md2c.SourceTree
import zio.ZIO
import zio.cli.Args
import zio.cli.Command
import zio.cli.Exists.Either
import zio.cli.Exists.Yes
import zio.cli.HelpDoc.*
import zio.cli.HelpDoc.Span.*
import zio.cli.Options
import zio.http.ZClient

case class Markdown2Confluence(
    sourceDir: File,
    cleanup: Boolean,
    renderOnly: Boolean,
    target: Option[File]
) extends ToolCommand {
  override def run(conf: Conf): ZIO[Any, Throwable, Unit] =
    if (renderOnly && cleanup) {
      ZIO.fail(
        new IllegalArgumentException(
          "--render-only and --cleanup cannot be used together"
        )
      )
    } else if (renderOnly) {
      target match {
        case None =>
          ZIO.fail(
            new IllegalArgumentException("--render-only requires --target")
          )
        case Some(dir) =>
          OfflineRender.render(sourceDir, dir)
      }
    } else {
      conf.confluenceConf match {
        case None => ZIO.fail(new Exception("No confluence config."))
        case Some(confluence) =>
          val md2cConf: md2c.Conf = conf.md2c.getOrElse(md2c.Conf.Empty)

          doRun(md2cConf).provide(
            ZClient.default,
            Client.layer(confluence),
            Publisher.layer(md2cConf)
          )
      }
    }

  def doRun(conf: md2c.Conf) = ZIO.logSpan("run") {
    for {
      publisher <- ZIO.service[Publisher]
      source <- SourceTree.from(sourceDir)
      finalConf <- conf.finalConf(source.conf)
      _ <- cleanup(finalConf).when(cleanup)
      _ <- ZIO.logInfo(s"Publishing ${source.root.source}")
      page <- publisher.publish(source)
      _ <- ZIO.logInfo(s"Published ${page.title}")
    } yield ()
  }

  def cleanup(conf: md2c.Conf.Final) = ZIO.logSpan("cleanup") {
    for {
      client <- ZIO.service[Client]
      user <- client.getCurrentUser()
      drafts <- client.getDraftPages(conf.spaceKey)
      draftPages <- ZIO.foreachPar(drafts)(d => client.getPage(d.id))
      toDelete = draftPages.filter(_.authorId == user.accountId)
      _ <- ZIO.logInfo(
        s"Deleting drafts:\n  -${toDelete.map(p => s"${p.id}: ${p.title}").mkString("\n  -")}"
      )
      deletes <- ZIO
        .foreachPar(toDelete)(p => client.deleteDraftPage(p.id))
    } yield deletes
  }

}

object Markdown2Confluence {

  private val sourceDir = Args.directory("sourceDir", Yes).atMost(1)
  private val cleanup =
    Options.boolean("cleanup") ?? "Remove drafts from the space."
  private val renderOnly =
    Options.boolean("render-only") ??
      "Render PlantUML, Mermaid, and D2 fences to png files. Does not publish."
  private val target =
    Options.directory("target", Either).optional ??
      "Directory for --render-only images."

  val command: Command[Markdown2Confluence] =
    Command("md2c", cleanup ++ renderOnly ++ target, sourceDir)
      .withHelp(
        blocks(
          h2("Markdown to Confluence"),
          p("Publish a directory of Markdown documents to Confluence."),
          p(
            spans(
              text("With "),
              code("--render-only --target DIR"),
              text(
                ", write one png per diagram kind and do not contact Confluence."
              )
            )
          )
        )
      )
      .map { case ((c, render, targetDir), s) =>
        Markdown2Confluence(
          sourceDir = s match {
            case Nil      => File.currentWorkingDirectory
            case dir :: _ => dir
          },
          cleanup = c,
          renderOnly = render,
          target = targetDir.map(File.apply)
        )
      }
}
