package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.*
import zio.test.*

object BracketLayoutSpec extends ZIOSpecDefault {

  def spec = suite("BracketLayout")(
    test("bracketRound for gf-1 matches server winners-round semantics") {
      assertTrue(
        BracketLayout.bracketRound("gf-1", bracketSize = 8) == Some(3),
        BracketLayout.bracketRound("gf-1", bracketSize = 16) == Some(4),
        BracketLayout.bracketRound("wb-2-1", bracketSize = 8) == Some(2),
        BracketLayout.bracketRound("lb-3-2", bracketSize = 8) == Some(3)
      )
    },
    test("roundOf delegates to bracketRound for grand final") {
      assertTrue(
        BracketLayout.roundOf("gf-1", bracketSize = 8) == 3,
        BracketLayout.roundOf("wb-1-1", bracketSize = 8) == 1
      )
    },
    test("groupLabel omits round number for grand final") {
      assertTrue(
        BracketLayout.groupLabel(BracketLayout.Section.GrandFinal, 3) ==
          "Grand Final",
        BracketLayout.groupLabel(BracketLayout.Section.Winners, 2) ==
          "Winners — round 2",
        BracketLayout.groupLabel(BracketLayout.Section.Losers, 4) ==
          "Losers — round 4"
      )
    },
    test("matchLabel delegates to groupLabel") {
      assertTrue(
        BracketLayout.matchLabel("gf-1", bracketSize = 8) == "Grand Final",
        BracketLayout.matchLabel("wb-2-1", bracketSize = 8) ==
          "Winners — round 2"
      )
    },
    test("groupMatches uses bracket size for grand final round grouping") {
      val gf = BracketMatch(
        id = "gf-1",
        playerA = None,
        playerB = None,
        state = BracketMatchState.Pending
      )
      val groups =
        BracketLayout.groupMatches(matches = List(gf), bracketSize = 8)
      assertTrue(
        groups.length == 1,
        groups.head.section == BracketLayout.Section.GrandFinal,
        groups.head.round == 3
      )
    },
    test("resultLabel shows bye for completed bye matches") {
      val byeMatch = BracketMatch(
        id = "wb-1-1",
        playerA = Some(Player("P1")),
        playerB = None,
        state = BracketMatchState.Completed,
        result = Some(MatchResult(scoreA = 1, scoreB = 0)),
        isBye = true
      )
      assertTrue(BracketLayout.resultLabel(byeMatch) == Some("bye"))
    },
    test("resultLabel shows score for normal completed matches") {
      val matchDef = BracketMatch(
        id = "wb-1-2",
        playerA = Some(Player("P2")),
        playerB = Some(Player("P3")),
        state = BracketMatchState.Completed,
        result = Some(MatchResult(scoreA = 7, scoreB = 4))
      )
      assertTrue(BracketLayout.resultLabel(matchDef) == Some("7–4"))
    },
    test("resultLabel is empty for pending matches") {
      val matchDef = BracketMatch(
        id = "wb-2-1",
        playerA = Some(Player("P1")),
        playerB = None,
        state = BracketMatchState.Pending
      )
      assertTrue(BracketLayout.resultLabel(matchDef) == None)
    },
    test("resultLabel shows bye for ghost structural byes without a result") {
      val ghostBye = BracketMatch(
        id = "lb-1-2",
        playerA = None,
        playerB = None,
        state = BracketMatchState.Completed,
        isBye = true
      )
      assertTrue(BracketLayout.resultLabel(ghostBye) == Some("bye"))
    }
  )
}
