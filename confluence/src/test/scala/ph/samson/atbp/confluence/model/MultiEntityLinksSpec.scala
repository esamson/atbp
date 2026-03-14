package ph.samson.atbp.confluence.model

import zio.test.*

object MultiEntityLinksSpec extends ZIOSpecDefault {

  override def spec = suite("MultiEntityLinks")(
    test("returns None when next is empty") {
      val links = MultiEntityLinks(
        base = "https://example.atlassian.net/wiki",
        next = None
      )

      assertTrue(links.nextUrl.isEmpty)
    },
    test("merges overlapping /wiki prefix and suffix") {
      val links = MultiEntityLinks(
        base = "https://example.atlassian.net/wiki",
        next = Some("/wiki/api/v2/pages")
      )

      assertTrue(
        links.nextUrl.contains(
          "https://example.atlassian.net/wiki/api/v2/pages"
        )
      )
    },
    test("avoids double slash at the join boundary") {
      val links = MultiEntityLinks(
        base = "https://example.atlassian.net/wiki/",
        next = Some("/wiki/api/v2/pages")
      )

      assertTrue(
        links.nextUrl.contains(
          "https://example.atlassian.net/wiki/api/v2/pages"
        )
      )
    },
    test("adds a slash when there is no overlap and no boundary slash") {
      val links = MultiEntityLinks(
        base = "https://example.atlassian.net/wiki",
        next = Some("api/v2/pages")
      )

      assertTrue(
        links.nextUrl.contains(
          "https://example.atlassian.net/wiki/api/v2/pages"
        )
      )
    },
    test("keeps existing single slash when next starts with slash") {
      val links = MultiEntityLinks(
        base = "https://example.atlassian.net",
        next = Some("/wiki")
      )

      assertTrue(
        links.nextUrl.contains("https://example.atlassian.net/wiki")
      )
    },
    test("keeps existing single slash when base ends with slash") {
      val links = MultiEntityLinks(
        base = "https://example.atlassian.net/",
        next = Some("wiki")
      )

      assertTrue(
        links.nextUrl.contains("https://example.atlassian.net/wiki")
      )
    },
    test("removes the largest exact overlap") {
      val links = MultiEntityLinks(
        base = "https://example.atlassian.net/wiki/rest/api",
        next = Some("/api/content")
      )

      assertTrue(
        links.nextUrl.contains(
          "https://example.atlassian.net/wiki/rest/api/content"
        )
      )
    }
  )
}
