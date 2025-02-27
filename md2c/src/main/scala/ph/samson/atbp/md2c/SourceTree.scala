package ph.samson.atbp.md2c

import better.files.File
import ph.samson.atbp.md2c.SourceTree.Node.Data
import ph.samson.atbp.md2c.SourceTree.Node.Directory
import ph.samson.atbp.md2c.SourceTree.Node.MarkdownBranch
import ph.samson.atbp.md2c.SourceTree.Node.MarkdownLeaf
import zio.Task
import zio.ZIO

import java.io.FileNotFoundException
import scala.util.matching.Regex

import SourceTree.*

/** The source documents.
  *
  * @param root
  *   the top of the document hierarchy
  */
case class SourceTree(root: Node, conf: SourceConf)

object SourceTree {

  enum Node(val name: String, val source: File) {

    /** A Directory is just a holder for other pages. */
    case Directory(override val name: String, dir: File, children: List[Node])
        extends Node(name, dir)

    /** A directory containing a markdown file with the same name (sans `.md`
      * extension) takes that markdown content for its own. Together, they are
      * treated as a branch node.
      *
      * Same as the Folder Notes idea in Obsidian.
      */
    case MarkdownBranch(
        override val name: String,
        content: File,
        children: List[Node]
    ) extends Node(name, content)

    /** Just markdown content. */
    case MarkdownLeaf(override val name: String, content: File)
        extends Node(name, content)

    case Data(override val name: String, path: File) extends Node(name, path)
  }

  def from(sourceDir: File): Task[SourceTree] = {

    // find the directory containing `.md2c.conf` file
    def sourceRoot(dir: File): Task[(File, SourceConf)] = for {
      confCheck <- SourceConf.get(dir)
      found <- confCheck match {
        case Some(conf) => ZIO.succeed((dir, conf))
        case None =>
          dir.parentOption match {
            case Some(parent) => sourceRoot(parent)
            case None =>
              ZIO.fail(
                new IllegalArgumentException(
                  s"No `.md2c.conf` file for $sourceDir or any of its ancestors"
                )
              )
          }
      }
    } yield found

    for {
      (root, conf) <-
        if (sourceDir.isDirectory ) sourceRoot(sourceDir)
        else ZIO.fail(new FileNotFoundException(sourceDir.pathAsString))
      node <- ZIO.attemptBlocking {
        def buildNode(path: File): Node = {
          if (path.isDirectory ) {
            val name = path.name
            val contentFile: Option[File] = List(
              s"$name.md",
              "README.md",
              "index.md"
            ).map(n => path / n).find(_.isRegularFile)

            contentFile match {
              case Some(content) =>
                MarkdownBranch(
                  name,
                  content,
                  path.children
                    .filterNot(ignored)
                    .filterNot(_.isSameFileAs(content))
                    .map(buildNode)
                    .toList
                    .sortBy(_.name)
                )
              case None =>
                Directory(
                  path.name,
                  path,
                  path.children
                    .filterNot(ignored)
                    .map(buildNode)
                    .toList
                    .sortBy(_.name)
                )
            } // end match
          } else {
            path.name match {
              case FileName.Markdown(name) => MarkdownLeaf(name, path)
              case other                   => Data(other, path)
            }
          }
        }

        SourceTree(buildNode(root), conf)
      }
    } yield node
  }

  val ignores: List[Regex] = List(
    """^\..+""".r // dotfiles
  )

  def ignored(path: File): Boolean = {
    ignores.find(_.matches(path.name)) match {
      case Some(_) => true
      case None    => false
    }
  }

  object FileName {
    val Markdown = """(.*).(?i:md)""".r
  }
}
