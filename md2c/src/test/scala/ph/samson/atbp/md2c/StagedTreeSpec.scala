package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.model.node.Extension
import com.atlassian.adf.model.node.Rule
import ph.samson.atbp.md2c.Parser.FrontMatter
import ph.samson.atbp.md2c.StagedTree.Page
import zio.ZIO
import zio.test.*

import java.time.Instant
import scala.jdk.CollectionConverters.*

object StagedTreeSpec extends ZIOSpecDefault {

  override def spec = suite("StagedTree")(
    test("Single Node") {
      for {
        source <- SourceTreeSpec.from("Single Node")
        result <- StagedTree.from(source)
      } yield assert(result)(inside {
        case StagedTree(
              Page("Single Node", source, FrontMatter.Empty, _, _, Nil)
            ) =>
          source.name == "Single Node.md"
      })
    },
    test("Single Child") {
      for {
        source <- SourceTreeSpec.from("Single Child")
        result <- StagedTree.from(source)
      } yield assert(result)(inside {
        case StagedTree(
              Page(
                "Single Child",
                parent,
                FrontMatter.Empty,
                _,
                _,
                List(Page("The Child", child, FrontMatter.Empty, _, _, Nil))
              )
            ) =>
          parent.isDirectory && child.isRegularFile
      })
    },
    test("Node with Child") {
      for {
        source <- SourceTreeSpec.from("Node with Child")
        result <- StagedTree.from(source)
      } yield assert(result)(inside {
        case StagedTree(
              Page(
                "Node with Child",
                parent,
                FrontMatter.Empty,
                _,
                _,
                List(Page("The Child", child, FrontMatter.Empty, _, _, Nil))
              )
            ) =>
          parent.isRegularFile && child.isRegularFile
      })
    },
    test("Multiple Children ignores unknown.child data file") {
      for {
        source <- SourceTreeSpec.from("Multiple Children")
        result <- StagedTree.from(source)
      } yield assert(result)(inside {
        case StagedTree(
              Page(
                "Multiple Children",
                parent,
                FrontMatter.Empty,
                _,
                _,
                List(
                  Page("Markdown Child", child, FrontMatter.Empty, _, _, Nil)
                )
              )
            ) =>
          // unknown.child data file is ignored
          parent.isRegularFile && child.isRegularFile
      })
    },
    test("Multiple Levels") {
      for {
        source <- SourceTreeSpec.from("Multiple Levels")
        result <- StagedTree.from(source)
      } yield assert(result)(inside {
        case StagedTree(
              Page(
                "Multiple Levels",
                parent1,
                FrontMatter.Empty,
                _,
                _,
                List(
                  Page("Level 1 Child", child1, FrontMatter.Empty, _, _, Nil),
                  Page(
                    "Level 2",
                    parent2,
                    FrontMatter.Empty,
                    _,
                    _,
                    List(
                      Page(
                        "Level 2 Child",
                        child2,
                        FrontMatter.Empty,
                        _,
                        _,
                        Nil
                      )
                    )
                  )
                )
              )
            ) =>
          parent1.isRegularFile &&
          child1.isRegularFile &&
          parent2.isDirectory &&
          child2.isRegularFile
      })
    },
    test("Empty Branch") {
      for {
        source <- SourceTreeSpec.from("Empty Branch")
        result <- StagedTree.from(source)
      } yield assert(result)(inside {
        case StagedTree(
              Page(
                "Empty Branch",
                root,
                _,
                _,
                _,
                List(
                  Page(
                    "Trees",
                    _,
                    _,
                    _,
                    _,
                    List(
                      Page(
                        "can",
                        _,
                        _,
                        _,
                        _,
                        List(
                          Page(
                            "go",
                            _,
                            _,
                            _,
                            _,
                            List(
                              Page(
                                "deep",
                                _,
                                _,
                                _,
                                _,
                                List(
                                  Page("Note", leaf, _, _, _, Nil)
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            ) =>
          root.isDirectory &&
          leaf.isRegularFile
      })
    },
    test(
      "directory page keeps its child macro before one literal source footer"
    ) {
      ZIO
        .acquireRelease(
          ZIO.attemptBlocking(File.newTemporaryDirectory())
        )(dir =>
          ZIO.attemptBlocking(dir.delete(swallowIOExceptions = true)).orDie
        )
        .flatMap { dir =>
          val node = SourceTree.Node.Directory("docs", dir, Nil)
          for {
            page <- Page.convertPage(node, Map.empty)
            contents = page.adf.content().asScala.toList
            text = page.adf.plainText
            rules = page.adf.allNodesOfType(classOf[Rule]).count()
          } yield assertTrue(
            (contents.head match {
              case _: Extension => true; case _ => false
            }),
            rules == 1,
            text.contains(
              "Do not edit here. This page is a generated directory listing."
            ),
            text.contains(
              "Git metadata unavailable: source is not in a Git worktree."
            )
          )
        }
    },
    test(
      "a literal source filename keeps its own footer when a sibling changes"
    ) {
      for {
        root <- GitFixtures.temp
        source <- ZIO.attemptBlocking {
          GitFixtures.init(root)
          GitFixtures.commitFile(
            root,
            "guide 1.md",
            "sibling\n",
            "Add sibling page",
            "2026-01-01T10:00:00Z"
          ): Unit
          GitFixtures.commitFile(
            root,
            "guide [1].md",
            "literal source\n",
            "Add literal source page",
            "2026-01-02T10:00:00Z"
          )
        }
        node = SourceTree.Node.MarkdownLeaf("guide", source)
        original <- Page.convertPage(node, Map.empty)
        _ <- ZIO.attemptBlocking((root / "guide 1.md").write("sibling edit\n"))
        unstaged <- Page.convertPage(node, Map.empty)
        _ <- ZIO.attemptBlocking(GitFixtures.run(root, "add", "guide 1.md"))
        staged <- Page.convertPage(node, Map.empty)
        _ <- ZIO.attemptBlocking(source.write("literal source edit\n"))
        edited <- Page.convertPage(node, Map.empty)
        originalText = original.adf.plainText
        editedText = edited.adf.plainText
      } yield assertTrue(
        originalText.contains("Source: guide [1].md"),
        originalText.contains("Add literal source page"),
        !originalText.contains("Add sibling page"),
        !originalText.contains("Source has uncommitted changes"),
        unstaged.adf.toString == original.adf.toString,
        unstaged.contentHash == original.contentHash,
        staged.adf.toString == original.adf.toString,
        staged.contentHash == original.contentHash,
        editedText.contains("Source has uncommitted changes"),
        editedText.contains("Add literal source page"),
        !editedText.contains("Add sibling page")
      )
    },
    suite("repository variants")(
      test("a touched but unchanged source hashes like an untouched checkout") {
        for {
          base <- GitFixtures.temp
          origin = base / "origin"
          touched = base / "touched"
          untouched = base / "untouched"
          _ <- ZIO.attemptBlocking {
            GitFixtures.history(origin)
            GitFixtures.clone(origin, touched)
            GitFixtures.clone(origin, untouched)
            (touched / "docs" / "guide.md").touch(
              Instant.now().plusSeconds(60)
            ): Unit
          }
          touchedPage <- Page.convertPage(leafNode(touched), Map.empty)
          untouchedPage <- Page.convertPage(leafNode(untouched), Map.empty)
        } yield assertTrue(
          !touchedPage.adf.plainText.contains("uncommitted"),
          touchedPage.adf.toString == untouchedPage.adf.toString,
          touchedPage.contentHash == untouchedPage.contentHash
        )
      },
      test("equivalent checkouts at different locations hash identically") {
        for {
          base <- GitFixtures.temp
          origin = base / "origin"
          first = base / "first"
          second = base / "nested" / "deeper" / "second"
          linked = base / "linked"
          _ <- ZIO.attemptBlocking {
            GitFixtures.history(origin)
            second.parent.createDirectoryIfNotExists(createParents = true)
            GitFixtures.clone(origin, first)
            GitFixtures.clone(origin, second)
            GitFixtures.run(first, "worktree", "add", "-q", linked.toString)
          }
          pages <- ZIO.foreach(List(first, second, linked)) { root =>
            for {
              leaf <- Page.convertPage(leafNode(root), Map.empty)
              dir <- Page.convertPage(
                SourceTree.Node.Directory("docs", root / "docs", Nil),
                Map.empty
              )
              top <- Page.convertPage(
                SourceTree.Node.Directory("root", root, Nil),
                Map.empty
              )
            } yield (leaf, dir, top)
          }
          (leaf1, dir1, top1) = pages.head
        } yield assertTrue(
          pages.forall { case (leaf, dir, top) =>
            leaf.contentHash == leaf1.contentHash &&
            leaf.adf.toString == leaf1.adf.toString &&
            dir.contentHash == dir1.contentHash &&
            dir.adf.toString == dir1.adf.toString &&
            top.contentHash == top1.contentHash &&
            top.adf.toString == top1.adf.toString
          },
          leaf1.adf.plainText.contains("Source: docs/guide.md"),
          dir1.adf.plainText.contains("Source directory: docs"),
          top1.adf.plainText.contains("Source directory: ."),
          // only displayed facts (the shared remote) leak a location, never a checkout
          List(first, second, linked).forall(checkout =>
            !leaf1.adf.toString.contains(checkout.path.toString)
          )
        )
      },
      test("each source in a tree uses its own linked worktree") {
        for {
          base <- GitFixtures.temp
          main = base / "main"
          linked = base / "linked"
          _ <- ZIO.attemptBlocking {
            GitFixtures.history(main)
            GitFixtures.run(main, "worktree", "add", "-q", linked.toString)
            GitFixtures.commitFile(
              linked,
              "docs/guide.md",
              "four\n",
              "Linked only",
              "2026-01-05T10:00:00Z"
            )
          }
          node = SourceTree.Node.Directory(
            "docs",
            main / "docs",
            List(
              SourceTree.Node.MarkdownLeaf("main", main / "docs" / "guide.md"),
              SourceTree.Node.MarkdownLeaf(
                "linked",
                linked / "docs" / "guide.md"
              )
            )
          )
          parent <- Page.convertPage(node, Map.empty)
          mainLeaf <- Page.convertPage(Page.children(node)(0), Map.empty)
          linkedLeaf <- Page.convertPage(Page.children(node)(1), Map.empty)
        } yield assertTrue(
          !parent.adf.plainText.contains("Linked only"),
          mainLeaf.adf.plainText.contains("Source: docs/guide.md"),
          !mainLeaf.adf.plainText.contains("Linked only"),
          linkedLeaf.adf.plainText.contains("Source: docs/guide.md"),
          linkedLeaf.adf.plainText.contains("Linked only"),
          mainLeaf.contentHash != linkedLeaf.contentHash
        )
      },
      test("shallow pages show a notice; directory pages never do") {
        for {
          base <- GitFixtures.temp
          origin = base / "origin"
          shallowA = base / "shallow-a"
          shallowB = base / "elsewhere" / "shallow-b"
          full = base / "full"
          _ <- ZIO.attemptBlocking {
            GitFixtures.history(origin)
            shallowB.parent.createDirectoryIfNotExists(createParents = true)
            GitFixtures.clone(origin, shallowA, "--depth", "3")
            GitFixtures.clone(origin, shallowB, "--depth", "3")
            GitFixtures.clone(origin, full)
          }
          leafA <- Page.convertPage(leafNode(shallowA), Map.empty)
          leafB <- Page.convertPage(leafNode(shallowB), Map.empty)
          leafFull <- Page.convertPage(leafNode(full), Map.empty)
          dirShallow <- Page.convertPage(
            SourceTree.Node.Directory("docs", shallowA / "docs", Nil),
            Map.empty
          )
          dirFull <- Page.convertPage(
            SourceTree.Node.Directory("docs", full / "docs", Nil),
            Map.empty
          )
        } yield assertTrue(
          leafA.adf.plainText.contains(
            "History may be incomplete: shallow checkout."
          ),
          leafA.adf.plainText.indexOf("Second") <
            leafA.adf.plainText.indexOf("shallow checkout"),
          leafA.adf.plainText.contains("Third"),
          !leafA.adf.plainText.contains("First"),
          leafA.contentHash == leafB.contentHash,
          leafA.adf.toString == leafB.adf.toString,
          !leafFull.adf.plainText.contains("shallow"),
          leafFull.contentHash != leafA.contentHash,
          !dirShallow.adf.plainText.contains("shallow"),
          !dirShallow.adf.plainText.contains("History"),
          dirShallow.contentHash == dirFull.contentHash
        )
      }
    )
  ) @@ GitFixtures.isolated

  private def leafNode(root: File) =
    SourceTree.Node.MarkdownLeaf("guide", root / "docs" / "guide.md")
}
