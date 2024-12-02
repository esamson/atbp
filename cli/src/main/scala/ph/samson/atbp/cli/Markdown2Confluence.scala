package ph.samson.atbp.cli

import better.files.File
import ph.samson.atbp.confluence.Client
import ph.samson.atbp.md2c
import ph.samson.atbp.md2c.Publisher
import ph.samson.atbp.md2c.SourceTree
import zio.ZIO
import zio.cli.Args
import zio.cli.Command
import zio.cli.Exists.Yes
import zio.cli.HelpDoc.*
import zio.cli.Options
import zio.http.ZClient

case class Markdown2Confluence(sourceDir: File, cleanup: Boolean)
    extends ToolCommand {
  override def run(conf: Conf): ZIO[Any, Throwable, Unit] = {
    conf.confluenceConf match
      case None => ZIO.fail(new Exception("No confluence config."))
      case Some(confluence) =>
        val md2cConf: md2c.Conf = conf.md2c.getOrElse(md2c.Conf.Empty)

        doRun(md2cConf).provide(
          ZClient.default,
          Client.layer(confluence),
          Publisher.layer(md2cConf)
        )
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

  val command: Command[Markdown2Confluence] =
    Command("md2c", cleanup, sourceDir)
      .withHelp(
        blocks(
          h2("Markdown to Confluence"),
          p("Publish a directory of Markdown documents to Confluence.")
        )
      )
      .map { (c, s) =>
        Markdown2Confluence(
          sourceDir = s match
            case Nil      => File.currentWorkingDirectory
            case dir :: _ => dir,
          cleanup = c
        )
      }
}
