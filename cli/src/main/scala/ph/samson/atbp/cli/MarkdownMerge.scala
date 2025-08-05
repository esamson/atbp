package ph.samson.atbp.cli

import better.files.File
import zio.Task
import zio.ZIO
import zio.cli.Args
import zio.cli.Command
import zio.cli.Exists.Either
import zio.cli.Exists.Yes
import zio.cli.Options

case class MarkdownMerge(title: String, target: File, sources: List[File])
    extends ToolCommand {
  override def run(conf: Conf): Task[Unit] = ZIO.logSpan("mdmerge") {
    for {
      lines <- ZIO.foreachPar(sources)(transform)
      result = s"# $title" :: lines.flatten
      outFile <- ZIO.attemptBlockingIO {
        target.overwrite(result.mkString("\n"))
      }
    } yield ()
  }

  def transform(source: File): Task[List[String]] = for {
    lines <- ZIO.attemptBlockingIO(source.lines.toList)
  } yield {
    val title = s"\n## ${source.nameWithoutExtension}\n"
    title :: lines.map { l =>
      if (l.startsWith("#")) {
        "#" + l
      } else {
        l
      }
    }
  }
}

object MarkdownMerge {

  private val title =
    Options.text("title") ?? "Document title for the merged result"
  private val target = Options.file("target", Either)
  private val sources = Args.file(Yes).atLeast(1)

  val command: Command[MarkdownMerge] =
    Command("mdmerge", title ++ target, sources)
      .map { case ((title, target), sources) =>
        MarkdownMerge(title, target, sources.map(identity))
      }
}
