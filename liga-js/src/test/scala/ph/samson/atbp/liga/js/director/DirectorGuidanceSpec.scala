package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.bracket.TournamentBounds
import zio.test.*

object DirectorGuidanceSpec extends ZIOSpecDefault {

  def spec = suite("DirectorGuidance")(
    suite("lockRosterHint")(
      test("warns when roster is below minimum") {
        assertTrue(
          DirectorGuidance.lockRosterHint(2) ==
            s"Need at least ${TournamentBounds.MinPlayers} players to lock (currently 2)."
        )
      },
      test("returns empty hint for valid roster size") {
        assertTrue(
          DirectorGuidance.lockRosterHint(3).isEmpty,
          DirectorGuidance.lockRosterHint(64).isEmpty
        )
      },
      test("warns when roster exceeds maximum") {
        assertTrue(
          DirectorGuidance.lockRosterHint(65) ==
            s"At most ${TournamentBounds.MaxPlayers} players allowed (currently 65)."
        )
      }
    ),
    test("friendlyApiError translates player count validation") {
      assertTrue(
        DirectorGuidance.friendlyApiError(
          TournamentBounds.invalidPlayerCountMessage(2)
        ) ==
          s"Roster must have ${TournamentBounds.MinPlayers}–${TournamentBounds.MaxPlayers} players"
      )
    }
  )
}
