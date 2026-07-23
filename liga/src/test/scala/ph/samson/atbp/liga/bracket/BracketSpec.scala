package ph.samson.atbp.liga.bracket

import ph.samson.atbp.liga.model.*
import zio.test.*

object BracketSpec extends ZIOSpecDefault {

  private def rating(name: String, r: Double, rd: Double): PlayerRating =
    PlayerRating(Player(name), r, rd, wins = 0, losses = 0)

  private def ratings(names: List[String]): List[PlayerRating] =
    names.zipWithIndex.map { case (name, i) =>
      rating(name, 2000 - i * 50, rd = 100)
    }

  private def findMatch(bracket: Bracket, id: String): BracketMatch =
    bracket.matches.find(_.id == id).get

  private def wb2Matches(bracket: Bracket): List[BracketMatch] =
    bracket.matches.filter(_.id.startsWith("wb-2-"))

  private def playerInMatch(matchDef: BracketMatch, name: String): Boolean =
    matchDef.playerA.contains(Player(name)) ||
      matchDef.playerB.contains(Player(name))

  def spec = suite("Bracket")(
    suite("Seeding")(
      test("orders players by rating descending") {
        val players = ratings(List("Alice", "Bob", "Carol", "Dave"))
        val seeds = Seeding.seedOrder(players)
        assertTrue(
          seeds.map(_.player.name) == List("Alice", "Bob", "Carol", "Dave")
        )
      },
      test("tie-break uses lower RD then alphabetical name") {
        val alice = rating("Alice", 1500, rd = 120)
        val bob = rating("Bob", 1500, rd = 80)
        val seeds = Seeding.seedOrder(List(alice, bob))
        assertTrue(seeds.map(_.player.name) == List("Bob", "Alice"))
      },
      test("bracket positions pair 1 vs N for 8 players") {
        val order = Seeding.bracketSlotSeeds(bracketSize = 8)
        assertTrue(order == List(1, 8, 4, 5, 2, 7, 3, 6))
      }
    ),
    suite("BracketGen")(
      test("generates 14 matches for 8 players") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players)
        assertTrue(
          bracket.size == 8,
          bracket.matches.size == 14
        )
      },
      test("first round winners bracket pairs top seed vs bottom seed") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players)
        val first = findMatch(bracket, "wb-1-1")
        assertTrue(
          first.playerA.contains(Player("P1")),
          first.playerB.contains(Player("P8"))
        )
      },
      test("12 players in 16 bracket auto-advances bye matches") {
        val players = ratings((1 to 12).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players)
        val byeMatches = bracket.matches.filter(m =>
          m.id.startsWith("wb-1-") && m.state == BracketMatchState.Completed
        )
        val byeWinners =
          byeMatches.flatMap(m => m.playerA.orElse(m.playerB)).map(_.name)
        assertTrue(
          bracket.size == 16,
          byeMatches.size == 4,
          byeWinners.sorted == List("P1", "P2", "P3", "P4")
        )
      },
      test("3 players in 4 bracket auto-advances bye match") {
        val players = ratings(List("P1", "P2", "P3"))
        val bracket = BracketGen.generate(players)
        val byeMatches = bracket.matches.filter(m =>
          m.id.startsWith("wb-1-") && m.state == BracketMatchState.Completed
        )
        assertTrue(
          bracket.size == 4,
          bracket.matches.size == 6,
          byeMatches.size == 1,
          byeMatches.head.id == "wb-1-1",
          byeMatches.head.playerA.contains(Player("P1"))
        )
      },
      test("3 players in 4 bracket stops bye cascade at wb-2") {
        val players = ratings(List("P1", "P2", "P3"))
        val bracket = BracketGen.generate(players)
        val bye = findMatch(bracket, "wb-1-1")
        val wb2 = wb2Matches(bracket)
        val p1Slots = wb2.count(playerInMatch(_, "P1"))
        val ready = bracket.matches.filter(_.state == BracketMatchState.Ready)
        assertTrue(
          bye.state == BracketMatchState.Completed,
          bye.isBye,
          bye.result.contains(MatchResult(scoreA = 1, scoreB = 0)),
          p1Slots == 1,
          wb2.forall(_.state == BracketMatchState.Pending),
          ready.size == 1,
          ready.head.id == "wb-1-2",
          findMatch(bracket, "gf-1").playerA.isEmpty,
          findMatch(bracket, "gf-1").playerB.isEmpty
        )
      },
      test("12 players in 16 bracket marks R1 byes and stops cascade") {
        val players = ratings((1 to 12).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players)
        val byeMatches = bracket.matches.filter(m =>
          m.id.startsWith("wb-1-") && m.state == BracketMatchState.Completed
        )
        val byeWinners =
          byeMatches.flatMap(m => m.playerA.orElse(m.playerB)).map(_.name)
        val wb2 = wb2Matches(bracket)
        val winnersInWb2 = List("P1", "P2", "P3", "P4").map { name =>
          wb2.count(playerInMatch(_, name))
        }
        assertTrue(
          bracket.size == 16,
          byeMatches.size == 4,
          byeMatches.forall(_.isBye),
          byeWinners.sorted == List("P1", "P2", "P3", "P4"),
          winnersInWb2 == List(1, 1, 1, 1),
          wb2.forall(_.state == BracketMatchState.Pending),
          findMatch(bracket, "gf-1").playerA.isEmpty,
          findMatch(bracket, "gf-1").playerB.isEmpty
        )
      },
      test("8 players in 8 bracket has no bye matches") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players)
        assertTrue(bracket.matches.forall(!_.isBye))
      },
      test(
        "3 players: losers structural bye after wb-1-2 leaves lb-2-1 ready after wb-2-1"
      ) {
        val players = ratings(List("P1", "P2", "P3"))
        val afterWb12 =
          Advancement
            .advance(BracketGen.generate(players), "wb-1-2", Player("P2"))
            .toOption
            .get
            .bracket
        val lb11 = findMatch(afterWb12, "lb-1-1")
        val lb21AfterWb12 = findMatch(afterWb12, "lb-2-1")
        val afterWb21 =
          Advancement
            .advance(afterWb12, "wb-2-1", Player("P1"))
            .toOption
            .get
            .bracket
        val lb21 = findMatch(afterWb21, "lb-2-1")
        assertTrue(
          lb11.state == BracketMatchState.Completed,
          lb11.isBye,
          lb21AfterWb12.playerA.contains(Player("P3")),
          lb21.state == BracketMatchState.Ready,
          lb21.playerA.contains(Player("P3")),
          lb21.playerB.contains(Player("P2")),
          findMatch(afterWb21, "gf-1").playerA.contains(Player("P1"))
        )
      },
      test("supports bracket sizes 4, 8, 16, 32, and 64") {
        val sizes = List(4, 8, 16, 32, 64)
        val checks = sizes.map { size =>
          val players = ratings((1 to size).map(i => s"P$i").toList)
          val bracket = BracketGen.generate(players)
          bracket.size == size && bracket.matches.size == 2 * size - 2
        }
        assertTrue(checks.forall(identity))
      }
    ),
    suite("Advancement")(
      test("winners bracket advancement marks next match ready") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players)
        val after1 =
          Advancement.advance(bracket, "wb-1-4", Player("P3")).toOption.get
        val after2 =
          Advancement
            .advance(after1.bracket, "wb-1-3", Player("P2"))
            .toOption
            .get
        val next = findMatch(after2.bracket, "wb-2-2")
        assertTrue(
          next.playerA.contains(Player("P2")),
          next.playerB.contains(Player("P3")),
          next.state == BracketMatchState.Ready
        )
      },
      test("losers drop from winners bracket into losers bracket") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players)
        val after =
          Advancement.advance(bracket, "wb-1-4", Player("P3")).toOption.get
        val lb1 = findMatch(after.bracket, "lb-1-2")
        assertTrue(lb1.playerB.contains(Player("P6")))
      },
      test("grand final becomes ready when both brackets resolve") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val played = playOutTournament(players, maxSteps = 100)
        val gf = findMatch(played, "gf-1")
        assertTrue(
          gf.state == BracketMatchState.Ready,
          gf.playerA.nonEmpty,
          gf.playerB.nonEmpty
        )
      }
    )
  )

  /** Play every ready match, always picking playerA as winner, until grand
    * final is ready.
    */
  private def playOutTournament(
      players: List[PlayerRating],
      maxSteps: Int
  ): Bracket = {
    def loop(bracket: Bracket, steps: Int): Bracket = {
      val gf = findMatch(bracket, "gf-1")
      if (gf.state == BracketMatchState.Ready || steps <= 0) {
        bracket
      } else {
        bracket.matches.find(_.state == BracketMatchState.Ready) match {
          case None       => bracket
          case Some(next) =>
            val winner = next.playerA.orElse(next.playerB).get
            Advancement.advance(bracket, next.id, winner) match {
              case Left(_)       => bracket
              case Right(result) => loop(result.bracket, steps - 1)
            }
        }
      }
    }
    loop(BracketGen.generate(players), maxSteps)
  }

}
