package ph.samson.atbp.c2md

import better.files.Resource
import dev.nthings.adf4j.AdfToMarkdown
import zio.ZIO
import zio.test.*

object ConverterSpec extends ZIOSpecDefault {
  override def spec = suite("Converter")(
    test("converts fixture ADF to markdown") {
      for {
        adfJson <- ZIO
          .fromOption(Resource.asString("adf/simple.json"))
          .orElseFail(new NoSuchElementException("adf/simple.json"))
        markdown = AdfToMarkdown.create().toMarkdown(adfJson)
      } yield assertTrue(
        markdown.contains("## Overview"),
        markdown.contains("**world**")
      )
    },
    test("wraps adf4j output with front matter") {
      val body = "## Overview\n\nHello **world**!\n"
      val result = Format.withFrontMatter("Test Page", body)
      assertTrue(
        result.contains("title: Test Page"),
        result.contains(body.stripTrailing())
      )
    }
  )
}
