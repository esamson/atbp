package ph.samson.atbp.confluence.model

import zio.Chunk
import zio.test.*
import zio.test.Assertion.*

object ChildPageSpec extends ZIOSpecDefault {

  private val codec = ChildPage.codec

  override def spec = suite("ChildPage")(
    test("decodes children payload when childPosition is absent") {
      val json =
        """{
          |  "results": [
          |    {
          |      "id": "42",
          |      "status": "current",
          |      "title": "Child",
          |      "spaceId": "7"
          |    }
          |  ],
          |  "_links": {
          |    "base": "https://example.atlassian.net/wiki/api/v2"
          |  }
          |}
          |""".stripMargin

      val decoded = codec.decode(Chunk.fromArray(json.getBytes))
      assert(decoded)(
        isRight(
          equalTo(
            MultiEntityResult(
              List(
                ChildPage(
                  id = "42",
                  status = "current",
                  title = "Child",
                  spaceId = "7"
                )
              ),
              MultiEntityLinks(
                "https://example.atlassian.net/wiki/api/v2",
                None
              )
            )
          )
        )
      )
    }
  )
}
