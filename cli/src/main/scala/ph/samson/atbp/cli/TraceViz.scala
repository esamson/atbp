package ph.samson.atbp.cli

import better.files.File
import ph.samson.atbp.traceviz.TefJson
import zio.ZIO
import zio.cli.Args
import zio.cli.Command
import zio.cli.Exists.Yes
import zio.cli.HelpDoc.*

case class TraceViz(source: File) extends ToolCommand {
  override def run(conf: Conf): ZIO[Any, Throwable, Unit] =
    ZIO.logSpan("TraceViz") {
      for {
        _ <- ZIO.logInfo(s"processing $source")
        _ <- TefJson.convert(source)
      } yield ()
    }
}

object TraceViz {
  private val source = Args.file("source", Yes)

  val command: Command[TraceViz] =
    Command("traceviz", source)
      .withHelp(
        blocks(
          h2("Trace Visualizer"),
          p(
            "Render trace timings for visualization in https://ui.perfetto.dev/"
          )
        )
      )
      .map(TraceViz(_))
}
