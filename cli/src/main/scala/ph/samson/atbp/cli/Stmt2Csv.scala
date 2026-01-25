package ph.samson.atbp.cli

import better.files.File
import ph.samson.atbp.stmt2csv.Extractor
import zio.Console
import zio.ZIO
import zio.cli.Args
import zio.cli.Command
import zio.cli.Exists.Either
import zio.cli.Exists.Yes
import zio.cli.HelpDoc.*
import zio.cli.Options

case class Stmt2Csv(source: File, target: Option[File], text: Boolean)
    extends ToolCommand {

  override def run(conf: Conf): ZIO[Any, Throwable, Unit] = if (text) {
    for {
      result <- Extractor.extractText(
        source,
        target match {
          case Some(value) => value
          case None        => source.sibling(s"${source.name}.txt")
        }
      )
      _ <- Console.printLine(s"$source text extracted to $result")
    } yield ()
  } else {
    for {
      result <- Extractor.extract(
        source,
        target match {
          case Some(value) => value
          case None        => source.sibling(s"${source.name}.csv")
        }
      )
      _ <- Console.printLine(s"$source extracted to $result")
    } yield ()
  }
}

object Stmt2Csv {
  private val source = Args.file("source", Yes)
  private val target = Options.file("target", Either).optional
  private val text = Options.boolean("text", "plain-text", true, "csv")

  val command: Command[Stmt2Csv] = Command("stmt2csv", target ++ text, source)
    .withHelp(
      blocks(
        h2("Extract bank statement PDF to CSV"),
        p("Read given source FILE and extract transaction entries into CSV.")
      )
    )
    .map { case ((target, text), source) =>
      Stmt2Csv(source, target.map(File.apply), text)
    }
}
