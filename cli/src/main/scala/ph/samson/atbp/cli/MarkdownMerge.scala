package ph.samson.atbp.cli

import better.files.File
import ph.samson.atbp.cli.MarkdownMerge.TableOfContents
import ph.samson.atbp.cli.MarkdownMerge.TableOfContents2
import zio.Task
import zio.ZIO
import zio.cli.Args
import zio.cli.Command
import zio.cli.Exists.Either
import zio.cli.Exists.Yes
import zio.cli.Options

case class MarkdownMerge(
    title: Option[String],
    target: File,
    sources: List[File]
) extends ToolCommand {
  override def run(conf: Conf): Task[Unit] = ZIO.logSpan("mdmerge") {
    for {
      lines <- ZIO.foreachPar(sources)(transform)
      result = title match {
        case None        => TableOfContents :: lines.flatten
        case Some(value) =>
          s"# $title" :: TableOfContents2 :: lines.flatten.map { l =>
            if (l.trim.startsWith("#")) {
              l.takeWhile(_.isWhitespace) + "#" + l.dropWhile(_.isWhitespace)
            } else {
              l
            }
          }
      }
      outFile <- ZIO.attemptBlockingIO {
        target.overwrite(result.mkString("\n"))
      }
    } yield ()
  }

  def transform(source: File): Task[List[String]] = for {
    lines <- ZIO.attemptBlockingIO(source.lines.toList)
  } yield {
    val title = s"\n# ${source.nameWithoutExtension}\n"
    title :: lines
  }
}

object MarkdownMerge {

  private val title =
    Options.text("title").optional ?? "Document title for the merged result"
  private val target = Options.file("target", Either)
  private val sources = Args.file(Yes).atLeast(1)

  private val TableOfContents =
    """```extension
      |extensionKey = toc
      |extensionType = com.atlassian.confluence.macro.core
      |parameters {
      |    macroParams {
      |        maxLevel {
      |            value = "3"
      |        }
      |    }
      |}
      |```""".stripMargin

  private val TableOfContents2 =
    """
      |```extension
      |extensionKey = toc
      |extensionType = com.atlassian.confluence.macro.core
      |parameters {
      |    macroParams {
      |        minLevel {
      |            value = "2"
      |        }
      |        maxLevel {
      |            value = "4"
      |        }
      |    }
      |}
      |```""".stripMargin

  val command: Command[MarkdownMerge] =
    Command("mdmerge", title ++ target, sources)
      .map { case ((title, target), sources) =>
        MarkdownMerge(title, target, sources.map(identity))
      }
}
