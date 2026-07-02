package ph.samson.atbp.c2md

import zio.test.*

object FormatSpec extends ZIOSpecDefault {
  override def spec = suite("Format")(
    test("wraps body with YAML front matter") {
      val result = Format.withFrontMatter("My Page", "## Section\n\nBody")
      assertTrue(
        result.startsWith("---\ntitle: My Page\n---\n"),
        result.endsWith("## Section\n\nBody")
      )
    },
    test("handles empty body") {
      val result = Format.withFrontMatter("Empty", "")
      assertTrue(result == "---\ntitle: Empty\n---\n")
    }
  )
}
