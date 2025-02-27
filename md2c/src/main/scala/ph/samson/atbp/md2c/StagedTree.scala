package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Heading
import ph.samson.atbp.md2c.Parser.FrontMatter
import ph.samson.atbp.md2c.Parser.Parsed
import ph.samson.atbp.md2c.SourceTree.Node
import ph.samson.atbp.md2c.SourceTree.Node.Directory
import ph.samson.atbp.md2c.SourceTree.Node.MarkdownBranch
import ph.samson.atbp.md2c.SourceTree.Node.MarkdownLeaf
import ph.samson.atbp.md2c.StagedTree.Page
import zio.Scope
import zio.Task
import zio.ZIO

import scala.jdk.CollectionConverters.*

case class StagedTree(root: Page)

object StagedTree {
  case class Page(
      name: String,
      source: File,
      frontMatter: FrontMatter,
      adf: Doc,
      contentHash: String,
      children: List[Page]
  ) {
    def title: String = frontMatter.title.getOrElse(name)

    /** If content starts with a top-level heading that is the same as the title
      * of the page, it shows up as redundant since Confluence renders the title
      * separately from the content.
      *
      * @return
      *   Page with no redundant heading.
      */
    def trimmedTitle: Page = {
      adf.content().asScala.toList match {
        case head :: next =>
          head match {
            case heading: Heading =>
              if (heading.level() == 1 && heading.toPlainText == title )
                copy(adf = Doc.doc(next.asJava)).trimmedTitle
              else this
            case _ => this
          }
        case Nil => this
      }
    }
  }

  def from(source: SourceTree) = for {
    root <- Page.convert(source.root)
  } yield StagedTree(root)

  object Page {

    def parse(node: Node): Task[Parsed] = node match {
      case Directory(name, _, _) =>
        ZIO.succeed(
          Parsed(FrontMatter.Empty, Doc.doc(Heading.h1(name)), "directory")
        )
      case MarkdownBranch(_, content, _) => Parser.parse(content)
      case MarkdownLeaf(_, content)      => Parser.parse(content)
      case unhandled =>
        ZIO.fail(
          new IllegalArgumentException(s"No ADF Doc for content: $unhandled")
        )
    }

    def children(node: Node): List[Node] = {
      val c = node match {
        case Node.Directory(_, _, children)      => children
        case Node.MarkdownBranch(_, _, children) => children
        case _                                   => Nil
      }

      c.filterNot(empty).collect {
        case c @ (_: Directory | _: MarkdownBranch | _: MarkdownLeaf) => c
      }
    }

    private def empty(node: Node): Boolean = node match {
      case Node.Directory(_, _, children) => children.forall(empty)
      case Node.MarkdownBranch(_, _, _)   => false
      case Node.MarkdownLeaf(_, _)        => false
      case Node.Data(_, _)                => true
    }

    def convert(node: Node): ZIO[Any & Scope, Throwable, Page] = {
      for {
        Parsed(frontMatter, sourceDoc, contentHash) <- parse(node)
        plantUmlRendered <- PlantUml.transform(sourceDoc)
        children <- ZIO.foreachPar(children(node))(convert)
      } yield Page(
        node.name,
        node.source,
        frontMatter,
        plantUmlRendered,
        contentHash,
        children
      )
    }
  }
}
