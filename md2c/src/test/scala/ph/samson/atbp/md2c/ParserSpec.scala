package ph.samson.atbp.md2c

import ph.samson.atbp.md2c.Parser.FrontMatter
import ph.samson.atbp.md2c.Parser.Parsed
import zio.test.*

object ParserSpec extends ZIOSpecDefault {

  override def spec = suite("Parser")(
    suite("splitFrontMatter")(
      test("No front matter") {
        val content = """Nothing
                        |to
                        |see
                        |here
                        |""".stripMargin
        val (fm, md) = Parser.splitFrontMatter(content)
        assertTrue(
          fm.isEmpty, {
            md ==
              """Nothing
                |to
                |see
                |here
                |""".stripMargin
          }
        )
      },
      test("Simple") {
        val content = """---
                        |a = b
                        |---
                        |Nothing
                        |to
                        |see
                        |here
                        |""".stripMargin
        val (fm, md) = Parser.splitFrontMatter(content)
        assertTrue(
          fm == "a = b", {
            md ==
              """Nothing
                |to
                |see
                |here
                |""".stripMargin
          }
        )
      },
      test("No ending") {
        val content = """---
                        |a = b
                        |
                        |Nothing
                        |to
                        |see
                        |here
                        |""".stripMargin
        val (fm, md) = Parser.splitFrontMatter(content)
        assertTrue(
          fm.isEmpty, {
            md ==
              """---
                |a = b
                |
                |Nothing
                |to
                |see
                |here
                |""".stripMargin
          }
        )
      },
      test("Must be at very start") {
        val content = """
                        |---
                        |a = b
                        |---
                        |Nothing
                        |to
                        |see
                        |here
                        |""".stripMargin
        val (fm, md) = Parser.splitFrontMatter(content)
        assertTrue(
          fm.isEmpty, {
            md == """
                    |---
                    |a = b
                    |---
                    |Nothing
                    |to
                    |see
                    |here
                    |""".stripMargin
          }
        )
      },
      test("Multiple lines") {
        val content = """---
                        |a = b
                        |c = d
                        |---
                        |Nothing
                        |to
                        |see
                        |here
                        |""".stripMargin
        val (fm, md) = Parser.splitFrontMatter(content)
        assertTrue(
          fm == """a = b
                  |c = d""".stripMargin, {
            md ==
              """Nothing
                |to
                |see
                |here
                |""".stripMargin
          }
        )
      }
    ),
    suite("parseFrontMatter")(
      test("Empty") {
        val frontMatter = ""
        for {
          info <- Parser.parseFrontMatter(frontMatter)
        } yield assertTrue(info == FrontMatter.Empty)
      },
      test("title") {
        val frontMatter = "title: 123"
        for {
          info <- Parser.parseFrontMatter(frontMatter)
        } yield assertTrue(info == FrontMatter(Some("123")))
      }
    ),
    suite("parse")(
      test("One") {
        val markdown = testMarkdown("One.md")
        for {
          Parsed(_, doc, _) <- Parser.parse(markdown)
        } yield {
          pprint.pprintln(s"doc: $doc")
          assertTrue(doc.isSupported)
        }
      },
      test("Sequence Diagram") {
        val markdown = testMarkdown("plantuml/Sequence Diagram.md")
        for {
          Parsed(_, doc, _) <- Parser.parse(markdown)
        } yield {
          pprint.pprintln(s"doc: $doc")
          assertTrue(doc.isSupported)
        }
      }
    )
  )

  private def testMarkdown(name: String) = {
    TestFiles("markdown") / name
  }
}
