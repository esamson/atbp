package ph.samson.atbp.liga.js.audience

import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.js.director.BracketLayout
import zio.test.*

object AudienceSpatialLayoutSpec extends ZIOSpecDefault {

  def spec = suite("AudienceSpatialLayout")(
    test("layout omits rounds with only hidden empty-future Pending matches") {
      val hidden = BracketMatch(
        id = "wb-1-1",
        playerA = None,
        playerB = None,
        state = BracketMatchState.Pending
      )
      val shown = BracketMatch(
        id = "wb-2-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready
      )
      val bands =
        AudienceSpatialLayout.layout(
          matches = List(hidden, shown),
          bracketSize = 8
        )
      assertTrue(
        bands.length == 1,
        bands.head.section == BracketLayout.Section.Winners,
        bands.head.columns.map(_.round) == List(2)
      )
    },
    test(
      "layout places unfinished columns left of completed columns in a band"
    ) {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      val unfinished = BracketMatch(
        id = "wb-2-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Started
      )
      val finished = BracketMatch(
        id = "wb-1-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Completed
      )
      val bands =
        AudienceSpatialLayout.layout(
          matches = List(finished, unfinished),
          bracketSize = 8
        )
      assertTrue(
        bands.length == 1,
        bands.head.columns.map(_.round) == List(2, 1)
      )
    },
    test(
      "layout places later unfinished columns left of earlier unfinished within a band"
    ) {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      val round2 = BracketMatch(
        id = "wb-2-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Ready
      )
      val round3 = round2.copy(id = "wb-3-1")
      val bands =
        AudienceSpatialLayout.layout(
          matches = List(round2, round3),
          bracketSize = 8
        )
      assertTrue(
        bands.length == 1,
        bands.head.columns.map(_.round) == List(3, 2)
      )
    },
    test("layout stacks Winners band above Losers band") {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      val wb = BracketMatch(
        id = "wb-1-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Ready
      )
      val lb = BracketMatch(
        id = "lb-1-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Ready
      )
      val bands =
        AudienceSpatialLayout.layout(
          matches = List(lb, wb),
          bracketSize = 8
        )
      assertTrue(
        bands.map(_.section) == List(
          BracketLayout.Section.Winners,
          BracketLayout.Section.Losers
        )
      )
    },
    test("layout sorts matches within a column by seed index not status") {
      val player = Some(Player("P1"))
      val ready = BracketMatch(
        id = "wb-2-4",
        playerA = player,
        playerB = None,
        state = BracketMatchState.Ready
      )
      val pending = BracketMatch(
        id = "wb-2-1",
        playerA = player,
        playerB = None,
        state = BracketMatchState.Pending
      )
      val started =
        pending.copy(id = "wb-2-3", state = BracketMatchState.Started)
      val completed =
        pending.copy(id = "wb-2-2", state = BracketMatchState.Completed)
      val bands =
        AudienceSpatialLayout.layout(
          matches = List(completed, started, pending, ready),
          bracketSize = 8
        )
      assertTrue(
        bands.length == 1,
        bands.head.columns.head.matches.map(_.id) ==
          List("wb-2-1", "wb-2-2", "wb-2-3", "wb-2-4")
      )
    },
    test("layout places gf-2 in a single slot despite seed index 2") {
      val gf2 = BracketMatch(
        id = "gf-2",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready
      )
      val bands =
        AudienceSpatialLayout.layout(matches = List(gf2), bracketSize = 8)
      assertTrue(
        bands.length == 1,
        bands.head.slotCount == 1,
        bands.head.columns.head.roundSize == 1,
        bands.head.columns.head.placed.map(p =>
          (p.matchDef.id, p.gridRowStart, p.gridRowSpan)
        ) ==
          List(("gf-2", 1, 1))
      )
    },
    test("layout uses BracketLayout.showForAudience visibility") {
      val hidden = BracketMatch(
        id = "wb-1-1",
        playerA = None,
        playerB = None,
        state = BracketMatchState.Pending
      )
      assertTrue(
        !BracketLayout.showForAudience(hidden),
        AudienceSpatialLayout
          .layout(
            matches = List(hidden),
            bracketSize = 8
          )
          .isEmpty
      )
    },
    suite("matchGridRow")(
      test("first packed match sits in row 1 spanning one slot") {
        assertTrue(
          AudienceSpatialLayout.matchGridRow(
            slotRank = 1,
            roundSize = 4,
            slotCount = 4
          ) == (1, 1)
        )
      },
      test("later-round match spans the two source slots") {
        assertTrue(
          AudienceSpatialLayout.matchGridRow(
            slotRank = 1,
            roundSize = 2,
            slotCount = 4
          ) == (1, 2),
          AudienceSpatialLayout.matchGridRow(
            slotRank = 2,
            roundSize = 2,
            slotCount = 4
          ) == (3, 2)
        )
      },
      test("final match spans every slot in the band") {
        assertTrue(
          AudienceSpatialLayout.matchGridRow(
            slotRank = 1,
            roundSize = 1,
            slotCount = 4
          ) == (1, 4)
        )
      }
    ),
    test("layout places a later-round match between its source pair") {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      def ready(id: String) =
        BracketMatch(
          id = id,
          playerA = player,
          playerB = opponent,
          state = BracketMatchState.Ready
        )
      val bands =
        AudienceSpatialLayout.layout(
          matches = List(
            ready("wb-1-1"),
            ready("wb-1-2"),
            ready("wb-1-3"),
            ready("wb-1-4"),
            ready("wb-2-1"),
            ready("wb-2-2"),
            ready("wb-3-1")
          ),
          bracketSize = 8
        )
      val winners = bands.head
      val byRound = winners.columns.map(c => c.round -> c).toMap
      assertTrue(
        winners.slotCount == 4,
        byRound(1).roundSize == 4,
        byRound(2).roundSize == 2,
        byRound(3).roundSize == 1,
        byRound(1).placed.map(p =>
          (p.matchDef.id, p.gridRowStart, p.gridRowSpan)
        ) ==
          List(
            ("wb-1-1", 1, 1),
            ("wb-1-2", 2, 1),
            ("wb-1-3", 3, 1),
            ("wb-1-4", 4, 1)
          ),
        byRound(2).placed.map(p =>
          (p.matchDef.id, p.gridRowStart, p.gridRowSpan)
        ) ==
          List(
            ("wb-2-1", 1, 2),
            ("wb-2-2", 3, 2)
          ),
        byRound(3).placed.map(p =>
          (p.matchDef.id, p.gridRowStart, p.gridRowSpan)
        ) ==
          List(("wb-3-1", 1, 4))
      )
    },
    test(
      "layout keeps hidden bye slots so remaining matches stay in seed rows"
    ) {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      def ready(id: String) =
        BracketMatch(
          id = id,
          playerA = player,
          playerB = opponent,
          state = BracketMatchState.Ready
        )
      def ghostBye(id: String) =
        BracketMatch(
          id = id,
          playerA = None,
          playerB = None,
          state = BracketMatchState.Completed,
          isBye = true
        )
      val bands =
        AudienceSpatialLayout.layout(
          matches = List(
            ready("wb-1-1"),
            ready("wb-1-2"),
            ghostBye("wb-1-3"),
            ready("wb-1-4"),
            ghostBye("wb-2-1"),
            ready("wb-2-2")
          ),
          bracketSize = 8
        )
      val byRound = bands.head.columns.map(c => c.round -> c).toMap
      assertTrue(
        bands.head.slotCount == 4,
        byRound(1).roundSize == 4,
        byRound(2).roundSize == 2,
        byRound(1).matches.map(_.id) == List("wb-1-1", "wb-1-2", "wb-1-4"),
        byRound(1).placed
          .find(_.matchDef.id == "wb-1-4")
          .map(p => (p.gridRowStart, p.gridRowSpan)) == Some((4, 1)),
        byRound(2).placed.map(p =>
          (p.matchDef.id, p.gridRowStart, p.gridRowSpan)
        ) ==
          List(("wb-2-2", 3, 2))
      )
    },
    test("layout places single elimination matches between source pairs") {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      def ready(id: String) =
        BracketMatch(
          id = id,
          playerA = player,
          playerB = opponent,
          state = BracketMatchState.Ready
        )
      val bands =
        AudienceSpatialLayout.layout(
          matches = List(
            ready("se-1-1"),
            ready("se-1-2"),
            ready("se-1-3"),
            ready("se-1-4"),
            ready("se-2-1"),
            ready("se-2-2"),
            ready("se-3-1")
          ),
          bracketSize = 8
        )
      val se = bands.head
      val byRound = se.columns.map(c => c.round -> c).toMap
      assertTrue(
        se.section == BracketLayout.Section.SingleElimination,
        se.slotCount == 4,
        byRound(1).roundSize == 4,
        byRound(2).roundSize == 2,
        byRound(3).roundSize == 1,
        byRound(1).placed.map(p =>
          (p.matchDef.id, p.gridRowStart, p.gridRowSpan)
        ) ==
          List(
            ("se-1-1", 1, 1),
            ("se-1-2", 2, 1),
            ("se-1-3", 3, 1),
            ("se-1-4", 4, 1)
          ),
        byRound(2).placed.map(p =>
          (p.matchDef.id, p.gridRowStart, p.gridRowSpan)
        ) ==
          List(
            ("se-2-1", 1, 2),
            ("se-2-2", 3, 2)
          ),
        byRound(3).placed.map(p =>
          (p.matchDef.id, p.gridRowStart, p.gridRowSpan)
        ) ==
          List(("se-3-1", 1, 4))
      )
    },
    test("layout aligns sixteen-player winners bracket across four rounds") {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      def ready(id: String) =
        BracketMatch(
          id = id,
          playerA = player,
          playerB = opponent,
          state = BracketMatchState.Ready
        )
      val round1 = (1 to 8).map(i => ready(f"wb-1-$i")).toList
      val round2 = (1 to 4).map(i => ready(f"wb-2-$i")).toList
      val round3 = (1 to 2).map(i => ready(f"wb-3-$i")).toList
      val bands =
        AudienceSpatialLayout.layout(
          matches = round1 ++ round2 ++ round3 ++ List(ready("wb-4-1")),
          bracketSize = 16
        )
      val winners = bands.head
      val byRound = winners.columns.map(c => c.round -> c).toMap
      assertTrue(
        winners.slotCount == 8,
        byRound(1).roundSize == 8,
        byRound(2).roundSize == 4,
        byRound(3).roundSize == 2,
        byRound(4).roundSize == 1,
        byRound(2).placed.map(p =>
          (p.matchDef.id, p.gridRowStart, p.gridRowSpan)
        ) ==
          List(
            ("wb-2-1", 1, 2),
            ("wb-2-2", 3, 2),
            ("wb-2-3", 5, 2),
            ("wb-2-4", 7, 2)
          ),
        byRound(3).placed.map(p =>
          (p.matchDef.id, p.gridRowStart, p.gridRowSpan)
        ) ==
          List(
            ("wb-3-1", 1, 4),
            ("wb-3-2", 5, 4)
          ),
        byRound(4).placed.map(p =>
          (p.matchDef.id, p.gridRowStart, p.gridRowSpan)
        ) ==
          List(("wb-4-1", 1, 8))
      )
    },
    test("layout keeps same-count losers rounds in matching slots") {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      def ready(id: String) =
        BracketMatch(
          id = id,
          playerA = player,
          playerB = opponent,
          state = BracketMatchState.Ready
        )
      val bands =
        AudienceSpatialLayout.layout(
          matches = List(
            ready("lb-1-1"),
            ready("lb-1-2"),
            ready("lb-2-1"),
            ready("lb-2-2"),
            ready("lb-3-1")
          ),
          bracketSize = 8
        )
      val byRound = bands.head.columns.map(c => c.round -> c).toMap
      assertTrue(
        bands.head.section == BracketLayout.Section.Losers,
        bands.head.slotCount == 2,
        byRound(1).placed.map(p =>
          (p.matchDef.id, p.gridRowStart, p.gridRowSpan)
        ) ==
          List(
            ("lb-1-1", 1, 1),
            ("lb-1-2", 2, 1)
          ),
        byRound(2).placed.map(p =>
          (p.matchDef.id, p.gridRowStart, p.gridRowSpan)
        ) ==
          List(
            ("lb-2-1", 1, 1),
            ("lb-2-2", 2, 1)
          ),
        byRound(3).placed.map(p =>
          (p.matchDef.id, p.gridRowStart, p.gridRowSpan)
        ) ==
          List(("lb-3-1", 1, 2))
      )
    }
  )
}
