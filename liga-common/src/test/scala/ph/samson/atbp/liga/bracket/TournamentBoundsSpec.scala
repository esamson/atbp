package ph.samson.atbp.liga.bracket

import zio.test.*

object TournamentBoundsSpec extends ZIOSpecDefault {

  def spec = suite("TournamentBounds")(
    test("validPlayerCount accepts 3 through 64") {
      assertTrue(
        (3 to 64).forall(TournamentBounds.validPlayerCount)
      )
    },
    test("validPlayerCount rejects below 3 and above 64") {
      assertTrue(
        !TournamentBounds.validPlayerCount(0),
        !TournamentBounds.validPlayerCount(2),
        !TournamentBounds.validPlayerCount(65)
      )
    },
    test("invalidPlayerCountMessage includes bounds and count") {
      assertTrue(
        TournamentBounds.invalidPlayerCountMessage(2) ==
          s"player count must be ${TournamentBounds.MinPlayers}–${TournamentBounds.MaxPlayers}: 2"
      )
    },
    test("bracketSize maps player counts to smallest fitting bracket") {
      assertTrue(
        TournamentBounds.bracketSize(3) == 4,
        TournamentBounds.bracketSize(4) == 4,
        TournamentBounds.bracketSize(5) == 8,
        TournamentBounds.bracketSize(7) == 8,
        TournamentBounds.bracketSize(8) == 8,
        TournamentBounds.bracketSize(9) == 16,
        TournamentBounds.bracketSize(33) == 64,
        TournamentBounds.bracketSize(64) == 64
      )
    }
  )
}
