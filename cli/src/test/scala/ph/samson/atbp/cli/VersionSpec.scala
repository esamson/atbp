package ph.samson.atbp.cli

import zio.test.*

object VersionSpec extends ZIOSpecDefault {

  override def spec = suite("versionString")(
    test("native property wins and gains a v prefix") {
      assertTrue(
        Main.versionString(Some("1.2.3"), Some("9.9.9")) == "v1.2.3",
        Main.versionString(Some("0.0.0+3-abcd"), None) == "v0.0.0+3-abcd"
      )
    },
    test("empty native property falls through to the jar version") {
      assertTrue(
        Main.versionString(Some(""), Some("1.2.3")) == "v1.2.3",
        Main.versionString(None, Some("1.2.3")) == "v1.2.3"
      )
    },
    test("neither source prints (dev)") {
      assertTrue(
        Main.versionString(None, None) == "(dev)",
        Main.versionString(Some(""), Some("")) == "(dev)",
        Main.versionString(None, Some("")) == "(dev)"
      )
    }
  )
}
