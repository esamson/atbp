package ph.samson.atbp.md2c

import better.files.*
import ph.samson.atbp.md2c.SourceTree.Node.Data
import ph.samson.atbp.md2c.SourceTree.Node.Directory
import ph.samson.atbp.md2c.SourceTree.Node.MarkdownBranch
import ph.samson.atbp.md2c.SourceTree.Node.MarkdownLeaf
import zio.Task
import zio.ZIO
import zio.test.*

object SourceTreeSpec extends ZIOSpecDefault {

  override def spec = suite("SourceTree")(
    test("Single Node") {
      for {
        tree <- testTree("Single Node")
        result <- SourceTree.from(tree)
      } yield assert(result)(inside {
        case SourceTree(
              MarkdownBranch("Single Node", _, Nil),
              _
            ) =>
          true
      })
    },
    test("Single Child") {
      for {
        tree <- testTree("Single Child")
        result <- SourceTree.from(tree)
      } yield assert(result)(inside {
        case SourceTree(
              Directory(
                "Single Child",
                _,
                List(
                  MarkdownLeaf("The Child", _)
                )
              ),
              _
            ) =>
          true
      })
    },
    test("Node with Child") {
      for {
        tree <- testTree("Node with Child")
        result <- SourceTree.from(tree)
      } yield assert(result)(inside {
        case SourceTree(
              MarkdownBranch(
                "Node with Child",
                _,
                List(
                  MarkdownLeaf("The Child", _)
                )
              ),
              _
            ) =>
          true
      })
    },
    test("Multiple Children") {
      for {
        tree <- testTree("Multiple Children")
        result <- SourceTree.from(tree)
      } yield assert(result)(inside {
        case SourceTree(
              MarkdownBranch(
                "Multiple Children",
                _,
                List(
                  MarkdownLeaf("Markdown Child", _),
                  Data("unknown.child", _)
                )
              ),
              _
            ) =>
          true
      })
    },
    test("Multiple Levels") {
      for {
        tree <- testTree("Multiple Levels")
        result <- SourceTree.from(tree)
      } yield assert(result)(inside {
        case SourceTree(
              MarkdownBranch(
                "Multiple Levels",
                _,
                List(
                  MarkdownLeaf("Level 1 Child", _),
                  Directory(
                    "Level 2",
                    _,
                    List(MarkdownLeaf("Level 2 Child", _))
                  )
                )
              ),
              _
            ) =>
          true
      })
    },
    suite("ignores")(
      test("dotfiles") {
        assertTrue(SourceTree.ignored(File(".git")))
      },
      test("regular files") {
        assertTrue(!SourceTree.ignored(File("README.md")))
      }
    )
  )

  def testTree(name: String): Task[File] =
    ZIO.attemptBlocking(TestFiles("trees") / name)

  def from(name: String): Task[SourceTree] = for {
    tree <- testTree(name)
    source <- SourceTree.from(tree)
  } yield source
}
