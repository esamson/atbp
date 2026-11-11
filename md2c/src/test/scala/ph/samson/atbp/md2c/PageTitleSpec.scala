package ph.samson.atbp.md2c

import better.files.File
import ph.samson.atbp.md2c.PageTitles.SourcePage
import zio.test.*

object PageTitleSpec extends ZIOSpecDefault {

  private def page(relativePath: String, logicalTitle: String): SourcePage = {
    val source = File(s"/fixture/$relativePath")
    SourcePage(source, relativePath, logicalTitle)
  }

  override def spec = suite("PageTitles")(
    test("a unique logical title is unchanged, including Existing root") {
      val pages = List(
        page("Guide.md", "Guide"),
        page("Overview.md", "Existing root")
      )
      val result = PageTitles.assign(pages)
      assertTrue(
        result(page("Guide.md", "Guide").source) == "Guide",
        result(page("Overview.md", "Existing root").source) == "Existing root"
      )
    },
    test("Left/Topic.md keeps Topic and Right/Topic.md receives one U+200B") {
      val pages = List(
        page("Left/Topic.md", "Topic"),
        page("Right/Topic.md", "Topic")
      )
      val result = PageTitles.assign(pages)
      val zws = PageTitles.ZeroWidthSpace
      assertTrue(
        result(page("Left/Topic.md", "Topic").source) == "Topic",
        result(page("Right/Topic.md", "Topic").source) == s"Topic$zws"
      )
    },
    test("three paths in lexicographic order keep bare, one, and two U+200B") {
      val pages = List(
        page("Alpha/Topic.md", "Topic"),
        page("Beta/Topic.md", "Topic"),
        page("Gamma/Topic.md", "Topic")
      )
      val result = PageTitles.assign(pages)
      val zws = "\u200B"
      assertTrue(
        result(page("Alpha/Topic.md", "Topic").source) == "Topic",
        result(page("Beta/Topic.md", "Topic").source) == s"Topic${zws * 1}",
        result(page("Gamma/Topic.md", "Topic").source) == s"Topic${zws * 2}"
      )
    },
    test("front matter title forms the group; file name does not") {
      val pages = List(
        page("Dir/README.md", "Same Title"),
        page("Other.md", "Same Title")
      )
      val result = PageTitles.assign(pages)
      val zws = PageTitles.ZeroWidthSpace
      assertTrue(
        result(page("Dir/README.md", "Same Title").source) == "Same Title",
        result(page("Other.md", "Same Title").source) == s"Same Title$zws"
      )
    },
    test("Topic and topic are different groups") {
      val pages = List(
        page("Upper/Topic.md", "Topic"),
        page("Lower/topic.md", "topic")
      )
      val result = PageTitles.assign(pages)
      assertTrue(
        result(page("Upper/Topic.md", "Topic").source) == "Topic",
        result(page("Lower/topic.md", "topic").source) == "topic"
      )
    },
    test("sort follows relative path for folder notes") {
      val pages = List(
        page("Zed/Dir.md", "Note"),
        page("Abc/README.md", "Note"),
        page("Mid/index.md", "Note")
      )
      val result = PageTitles.assign(pages)
      val zws = "\u200B"
      assertTrue(
        result(page("Abc/README.md", "Note").source) == "Note",
        result(page("Mid/index.md", "Note").source) == s"Note${zws * 1}",
        result(page("Zed/Dir.md", "Note").source) == s"Note${zws * 2}"
      )
    }
  )
}
