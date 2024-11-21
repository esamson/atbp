package ph.samson.atbp.md2c

import ph.samson.atbp.md2c.Parser.FrontMatter
import ph.samson.atbp.md2c.StagedTree.Page
import zio.Scope
import zio.test.*

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
    }
  )
}
