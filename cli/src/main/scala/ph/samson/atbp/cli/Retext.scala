package ph.samson.atbp.cli

import better.files.File
import ph.samson.atbp.retext.Reencoder
import zio.Console
import zio.ZIO
import zio.cli.Args
import zio.cli.Command
import zio.cli.Exists.Either
import zio.cli.Exists.Yes
import zio.cli.HelpDoc.*
import zio.cli.Options

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

case class Retext(
    sources: List[File],
    targetDir: Option[File],
    sourceCharset: Option[Charset],
    targetCharset: Option[Charset]
) extends ToolCommand {
  override def run(conf: Conf): ZIO[Any, Throwable, Unit] = {
    val sourceCs = sourceCharset.getOrElse(better.files.DefaultCharset)
    val targetCs = targetCharset.getOrElse(StandardCharsets.UTF_8)
    val tasks = for (source <- sources) yield {
      val target = targetDir match {
        case Some(dir) => dir / source.name
        case None      => source.sibling(s"$targetCs.${source.name}")
      }
      Reencoder.transform(source, sourceCs, target, targetCs).as(target)
    }

    for {
      targets <- ZIO.collectAllPar(tasks)
      _ <- Console.printLine(
        s"Reencoded to $targetCs:\n  ${targets.mkString("\n  ")}"
      )
    } yield ()
  }
}

object Retext {
  private val sources = Args.file("source", Yes).atLeast(1)
  private val targetDir = Options.directory("target", Either).optional
  private val sourceCharset =
    Options.text("source-charset").map(Charset.forName).optional
  private val targetCharset =
    Options.text("target-charset").map(Charset.forName).optional

  val command: Command[Retext] =
    Command("retext", targetDir ++ sourceCharset ++ targetCharset, sources)
      .withHelp(
        blocks(
          h2("Reencode text files"),
          p(
            "Read the given text FILE(s) and rewrite to a new file using the standard platform line separator."
          )
        )
      )
      .map { case ((td, sc, tc), s) =>
        Retext(s.map(File.apply), td.map(File.apply), sc, tc)
      }
}
