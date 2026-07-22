package ph.samson.atbp.liga.bracket

import zio.test.*

object RaceToScopesSpec extends ZIOSpecDefault {

  def spec = suite("RaceToScopes")(
    suite("keyForMatch")(
      test("maps winners match ids to wb scope keys") {
        assertTrue(
          RaceToScopes.keyForMatch("wb-3-1") == Some("wb-3"),
          RaceToScopes.keyForMatch("wb-1-4") == Some("wb-1")
        )
      },
      test("maps losers match ids to lb scope keys") {
        assertTrue(
          RaceToScopes.keyForMatch("lb-4-2") == Some("lb-4"),
          RaceToScopes.keyForMatch("lb-1-1") == Some("lb-1")
        )
      },
      test("maps grand final to gf scope key") {
        assertTrue(RaceToScopes.keyForMatch("gf-1") == Some("gf"))
      },
      test("returns None for unknown match ids") {
        assertTrue(
          RaceToScopes.keyForMatch("unknown").isEmpty,
          RaceToScopes.keyForMatch("wb-3").isEmpty
        )
      }
    ),
    suite("requiredKeys")(
      test("requiredKeys(3) returns five scope keys for size-4 bracket") {
        assertTrue(
          RaceToScopes.requiredKeys(3) == List(
            "wb-1",
            "wb-2",
            "lb-1",
            "lb-2",
            "gf"
          )
        )
      },
      test("requiredKeys(4) returns five scope keys for size-4 bracket") {
        assertTrue(
          RaceToScopes.requiredKeys(4) == List(
            "wb-1",
            "wb-2",
            "lb-1",
            "lb-2",
            "gf"
          )
        )
      },
      test("requiredKeys(8) returns eight scope keys") {
        assertTrue(
          RaceToScopes.requiredKeys(8) == List(
            "wb-1",
            "wb-2",
            "wb-3",
            "lb-1",
            "lb-2",
            "lb-3",
            "lb-4",
            "gf"
          )
        )
      },
      test("requiredKeys(16) returns eleven scope keys") {
        assertTrue(
          RaceToScopes.requiredKeys(16) == List(
            "wb-1",
            "wb-2",
            "wb-3",
            "wb-4",
            "lb-1",
            "lb-2",
            "lb-3",
            "lb-4",
            "lb-5",
            "lb-6",
            "gf"
          )
        )
      },
      test("requiredKeys(32) returns fourteen scope keys") {
        assertTrue(
          RaceToScopes.requiredKeys(32) ==
            (1 to 5).map(n => s"wb-$n").toList ++
            (1 to 8).map(n => s"lb-$n").toList :+
            "gf"
        )
      },
      test("requiredKeys(64) returns seventeen scope keys") {
        assertTrue(
          RaceToScopes.requiredKeys(64) ==
            (1 to 6).map(n => s"wb-$n").toList ++
            (1 to 10).map(n => s"lb-$n").toList :+
            "gf"
        )
      }
    ),
    suite("scopeLabel")(
      test("labels winners scopes with round number") {
        val label = RaceToScopes.scopeLabel("wb-3")
        assertTrue(
          label.section == RaceToScopes.Section.Winners,
          label.roundLabel == "Round 3"
        )
      },
      test("labels losers scopes with round number") {
        val label = RaceToScopes.scopeLabel("lb-4")
        assertTrue(
          label.section == RaceToScopes.Section.Losers,
          label.roundLabel == "Round 4"
        )
      },
      test("labels grand final scope") {
        val label = RaceToScopes.scopeLabel("gf")
        assertTrue(
          label.section == RaceToScopes.Section.GrandFinal,
          label.roundLabel == "Grand Final"
        )
      }
    )
  )
}
