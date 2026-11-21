package ph.samson.atbp.md2c

import com.atlassian.adf.model.node.CodeBlock
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Paragraph
import com.atlassian.adf.model.node.Rule
import com.atlassian.adf.model.node.Text
import com.atlassian.adf.model.node.`type`.DocContent
import zio.Task

import java.nio.charset.StandardCharsets.UTF_8
import scala.jdk.CollectionConverters.*

object SourceFooter {
  case class Rendered(adf: Doc, hashInput: String)

  private val formatVersion = "md2c-source-footer-v1"
  private val DirectoryWarning =
    "Do not edit here. This page is a generated directory listing."
  private val MarkdownWarning =
    "Do not edit here. This page was generated from markdown source."
  private val UncommittedNotice =
    "Source has uncommitted changes; history below contains committed changes only."
  private val ShallowNotice = "History may be incomplete: shallow checkout."
  private val StatusUnavailableNotice = "Source change status unavailable."

  /** One visible footer line. The ADF block and the canonical hash fields are
    * both derived from it, so a visible line cannot exist without being hashed.
    */
  private enum Line {
    case HorizontalRule
    case Note(text: String)
    case Command(command: String)
    case Labeled(label: String, value: String)
    case Commit(entry: GitMetadata.HistoryEntry)

    def block: DocContent = this match {
      case HorizontalRule        => Rule.hr()
      case Note(text)            => Paragraph.paragraph(Text.text(text))
      case Command(command)      => CodeBlock.codeBlock(command)
      case Labeled(label, value) =>
        Paragraph.paragraph(Text.text(s"$label "), Text.text(value).code())
      case Commit(entry) =>
        Paragraph.paragraph(
          Text.text(s"${entry.date} | ${entry.author} | "),
          Text.text(entry.hash).code(),
          Text.text(s" | ${entry.subject}")
        )
    }

    def fields: List[String] = this match {
      case HorizontalRule        => Nil
      case Note(text)            => List(text)
      case Command(command)      => List(command)
      case Labeled(label, value) => List(label, value)
      case Commit(entry)         =>
        List(entry.date, entry.author, entry.hash, entry.subject)
    }
  }

  private def render(lines: List[Line]): Rendered =
    Rendered(
      Doc.doc(lines.map(_.block).asJava),
      frame(formatVersion :: lines.flatMap(_.fields))
    )

  private def cloneLine(dir: GitMetadata.DirectoryInfo): Line =
    dir.cloneCommand match {
      case Some(command) => Line.Command(command)
      case None          =>
        Line.Note(
          dir.cloneNotice.getOrElse(GitMetadata.CloneLookupFailedNotice)
        )
    }

  private def pathText(relativePath: Option[String]): String =
    relativePath match {
      case Some("")   => "."
      case Some(path) => path
      case None       => "Repository-relative source path unavailable."
    }

  def markdown(info: GitMetadata.MarkdownInfo): Rendered =
    render(markdownLines(info))

  private def markdownLines(info: GitMetadata.MarkdownInfo): List[Line] = {
    val dir = info.directory
    val head = List(Line.HorizontalRule, Line.Note(MarkdownWarning))
    dir.gitNotice match {
      case Some(notice) => head :+ Line.Note(notice)
      case None         =>
        val status = info.changeStatus match {
          case GitMetadata.ChangeStatus.Uncommitted =>
            List(Line.Note(UncommittedNotice))
          case GitMetadata.ChangeStatus.Unavailable =>
            List(Line.Note(StatusUnavailableNotice))
          case GitMetadata.ChangeStatus.Clean => Nil
        }
        val history = info.historyNotice match {
          case Some(notice)                 => List(Line.Note(notice))
          case None if info.history.isEmpty =>
            List(Line.Note("No committed history for this source file."))
          case None =>
            val shallow =
              if (info.shallow) List(Line.Note(ShallowNotice)) else Nil
            Line.Note("Recent source history (up to 5 commits):") ::
              info.history.map(Line.Commit(_)) ++ shallow
        }
        head ++ List(
          cloneLine(dir),
          Line.Labeled("Source:", pathText(dir.relativePath))
        ) ++ status ++ history
    }
  }

  def directory(info: GitMetadata.DirectoryInfo): Rendered =
    render(directoryLines(info))

  private def directoryLines(info: GitMetadata.DirectoryInfo): List[Line] = {
    val head = List(Line.HorizontalRule, Line.Note(DirectoryWarning))
    info.gitNotice match {
      case Some(notice) => head :+ Line.Note(notice)
      case None         =>
        head ++ List(
          cloneLine(info),
          Line.Labeled("Source directory:", pathText(info.relativePath))
        )
    }
  }

  /** Append the footer blocks to the document and fold its hash input into the
    * content hash.
    */
  def attach(
      doc: Doc,
      contentHash: String,
      footer: Rendered
  ): Task[(Doc, String)] =
    Parser
      .computeHash(frame(List(contentHash, footer.hashInput)))
      .map(hash =>
        (
          Doc.doc(
            (doc.content().asScala.toList ++
              footer.adf.content().asScala.toList).asJava
          ),
          hash
        )
      )

  private[md2c] def frame(fields: List[String]): String =
    fields.map { field =>
      val bytes = field.getBytes(UTF_8)
      s"${bytes.length}:$field"
    }.mkString
}
