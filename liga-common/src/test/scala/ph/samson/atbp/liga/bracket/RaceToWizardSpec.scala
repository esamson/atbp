package ph.samson.atbp.liga.bracket

import zio.test.*

object RaceToWizardSpec extends ZIOSpecDefault {

  private val eightPlayerKeys = RaceToScopes.requiredKeys(8)

  private def mapAt(state: RaceToWizard.State, key: String): Int =
    state.raceToByScope(key)

  def spec = suite("RaceToWizard")(
    test("initialState(3) pre-fills all scopes for size-4 bracket") {
      val keys = RaceToScopes.requiredKeys(3)
      val state = RaceToWizard.initialState(3)
      assertTrue(
        keys == List("wb-1", "wb-2", "lb-1", "lb-2", "gf"),
        keys.forall(key => mapAt(state, key) == 7),
        !state.gfPinned
      )
    },
    test("initialState(8) pre-fills all scopes with synced losers and gf") {
      val state = RaceToWizard.initialState(8)
      assertTrue(
        eightPlayerKeys.forall(key => mapAt(state, key) == 7),
        !state.gfPinned
      )
    },
    test("editing wb-1 cascades winners, losers, and unpinned gf") {
      val initial = RaceToWizard.initialState(8)
      val edited = RaceToWizard.applyEdit(initial, "wb-1", 5, playerCount = 8)
      assertTrue(
        mapAt(edited, "wb-1") == 5,
        mapAt(edited, "wb-2") == 5,
        mapAt(edited, "wb-3") == 5,
        mapAt(edited, "lb-1") == 5,
        mapAt(edited, "lb-2") == 5,
        mapAt(edited, "lb-3") == 5,
        mapAt(edited, "lb-4") == 5,
        mapAt(edited, "gf") == 5,
        !edited.gfPinned
      )
    },
    test("editing lb-k cascades only the losers section") {
      val initial = RaceToWizard.initialState(8)
      val edited = RaceToWizard.applyEdit(initial, "lb-2", 5, playerCount = 8)
      assertTrue(
        mapAt(edited, "wb-1") == 7,
        mapAt(edited, "wb-2") == 7,
        mapAt(edited, "wb-3") == 7,
        mapAt(edited, "lb-1") == 7,
        mapAt(edited, "lb-2") == 5,
        mapAt(edited, "lb-3") == 5,
        mapAt(edited, "lb-4") == 5,
        mapAt(edited, "gf") == 7
      )
    },
    test("editing gf pins grand final against later winners edits") {
      val initial = RaceToWizard.initialState(8)
      val withGf = RaceToWizard.applyEdit(initial, "gf", 9, playerCount = 8)
      val afterWb2 = RaceToWizard.applyEdit(withGf, "wb-2", 5, playerCount = 8)
      assertTrue(
        withGf.gfPinned,
        mapAt(withGf, "gf") == 9,
        mapAt(afterWb2, "wb-2") == 5,
        mapAt(afterWb2, "wb-3") == 5,
        mapAt(afterWb2, "gf") == 9,
        afterWb2.gfPinned
      )
    },
    test("editing wb-k updates unpinned gf to wb-N") {
      val initial = RaceToWizard.initialState(8)
      val edited = RaceToWizard.applyEdit(initial, "wb-2", 5, playerCount = 8)
      assertTrue(
        mapAt(edited, "wb-1") == 7,
        mapAt(edited, "wb-2") == 5,
        mapAt(edited, "wb-3") == 5,
        mapAt(edited, "gf") == 5
      )
    },
    test("loadState infers gfPinned when gf differs from wb-N") {
      val serverMap =
        RaceToWizard.initialState(8).raceToByScope.updated("gf", 9)
      val loaded = RaceToWizard.loadState(serverMap, playerCount = 8)
      assertTrue(loaded.gfPinned, mapAt(loaded, "gf") == 9)
    },
    test("loadState leaves gf unpinned when gf matches wb-N") {
      val serverMap = RaceToWizard.initialState(8).raceToByScope
      val loaded = RaceToWizard.loadState(serverMap, playerCount = 8)
      assertTrue(!loaded.gfPinned)
    },
    test("spec 8-player differentiated scenario") {
      val step1 = RaceToWizard.applyEdit(
        RaceToWizard.initialState(8),
        "lb-1",
        5,
        playerCount = 8
      )
      val step2 = RaceToWizard.applyEdit(step1, "wb-2", 5, playerCount = 8)
      val step3 = RaceToWizard.applyEdit(step2, "gf", 9, playerCount = 8)
      assertTrue(
        mapAt(step1, "lb-1") == 5,
        mapAt(step1, "lb-4") == 5,
        mapAt(step1, "wb-3") == 7,
        mapAt(step2, "wb-1") == 7,
        mapAt(step2, "wb-2") == 5,
        mapAt(step2, "wb-3") == 5,
        mapAt(step2, "gf") == 5,
        mapAt(step3, "gf") == 9,
        step3.gfPinned
      )
    }
  )
}
