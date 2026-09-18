package ph.samson.atbp.md2c

import zio.test.*

object FooterSpec extends ZIOSpecDefault {

  override def spec = suite("Footer")(
    test("hashes the rendered footer definition") {
      for {
        expected <- Parser.computeHash(
          s"source hash\n${Footer.Title}\n${Footer.Body}"
        )
        actual <- Footer.contentHash("source hash")
      } yield assertTrue(actual == expected)
    }
  )
}
