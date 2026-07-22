package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.bracket.BracketGen
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.testsupport.RaceToTestSupport
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.test.*

import java.time.Instant

object TournamentSpec extends ZIOSpecDefault {

  private val at = Instant.parse("2026-03-15T18:00:00Z")

  private def rating(name: String, r: Double): PlayerRating =
    PlayerRating(Player(name), r, rd = 100, wins = 0, losses = 0)

  private val eightPlayerRatings: List[PlayerRating] =
    (1 to 8).map(i => rating(s"P$i", 1700 - i * 10)).toList

  private def seededState(raceToByScope: Map[String, Int]): TournamentState = {
    val bracket = BracketGen.generate(eightPlayerRatings)
    TournamentState(
      name = "Spring Open",
      players = eightPlayerRatings.map(_.player),
      bracket = Some(bracket),
      frozenRatings = eightPlayerRatings.map(r => r.player -> r).toMap,
      raceToByScope = raceToByScope
    )
  }

  private def seededState(): TournamentState =
    seededState(RaceToTestSupport.uniformRaceTo(8))

  private def seededEvents(state: TournamentState): List[TournamentEvent] = {
    val players = eightPlayerRatings.map(_.player)
    val raceToEvents =
      RaceToTestSupport.raceToSetEvents(playerCount = 8, startSeq = 4, at = at)
    List(
      TournamentEvent.Created(
        seq = 1,
        at = at,
        payload = TournamentCreatedPayload(
          name = state.name,
          players = Nil
        )
      ),
      TournamentEvent.PlayersSet(
        seq = 2,
        at = at,
        payload = PlayersSetPayload(players = players)
      ),
      TournamentEvent.PlayersLocked(
        seq = 3,
        at = at,
        payload = PlayersLockedPayload()
      )
    ) ++ raceToEvents :+ TournamentEvent.BracketSeeded(
      seq = 12,
      at = at,
      payload = BracketSeededPayload(
        frozenRatings = eightPlayerRatings,
        bracket = state.bracket.get
      )
    )
  }

  private def matchOf(state: TournamentState, id: String): BracketMatch =
    state.bracket.flatMap(_.matches.find(_.id == id)).get

  private def withMatch(
      state: TournamentState,
      id: String
  )(
      update: BracketMatch => BracketMatch
  ): TournamentState =
    state.copy(
      bracket = state.bracket.map { bracket =>
        bracket.copy(
          matches = bracket.matches.map { matchDef =>
            if (matchDef.id == id) {
              update(matchDef)
            } else {
              matchDef
            }
          }
        )
      }
    )

  def spec = suite("Tournament")(
    suite("ready")(
      test("MatchReady computes handicap suggestion from frozen ratings") {
        val state = seededState()
        val result = Tournament.ready(state, "wb-1-1", seq = 3, at)
        assertTrue(
          result.isRight,
          result.toOption.get.payload.matchId == "wb-1-1",
          result.toOption.get.payload.handicapSuggested >= 0
        )
      },
      test("ready rejects pending matches without both players") {
        val state = seededState()
        val pending =
          state.bracket.get.matches
            .find(_.state == BracketMatchState.Pending)
            .get
        assertTrue(Tournament.ready(state, pending.id, seq = 3, at).isLeft)
      },
      test("ready rejects started matches") {
        val state = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Started,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        assertTrue(Tournament.ready(state, "wb-1-1", seq = 3, at).isLeft)
      }
    ),
    suite("handicap")(
      test("HandicapApplied can differ from suggested") {
        val readyState = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Ready,
            handicapSuggested = Some(2)
          )
        }
        val result =
          Tournament.applyHandicap(
            readyState,
            "wb-1-1",
            handicap = 3,
            seq = 4,
            at
          )
        assertTrue(
          result.isRight,
          result.toOption.get.payload.handicapApplied == 3
        )
      },
      test("handicap rejects values above race-to cap") {
        val readyState = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Ready,
            handicapSuggested = Some(2)
          )
        }
        val result =
          Tournament.applyHandicap(
            readyState,
            "wb-1-1",
            handicap = 6,
            seq = 4,
            at
          )
        assertTrue(
          result.isLeft,
          result.left.toOption.get.message
            .contains("handicap must be at most 5")
        )
      },
      test("handicap rejects negative values") {
        val readyState = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Ready,
            handicapSuggested = Some(2)
          )
        }
        assertTrue(
          Tournament
            .applyHandicap(readyState, "wb-1-1", handicap = -1, seq = 4, at)
            .isLeft
        )
      },
      test("handicap rejects started matches") {
        val state = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Started,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        assertTrue(
          Tournament
            .applyHandicap(state, "wb-1-1", handicap = 3, seq = 4, at)
            .isLeft
        )
      }
    ),
    suite("start")(
      test("MatchStarted requires handicap to be applied first") {
        assertTrue(
          Tournament.start(seededState(), "wb-1-1", seq = 5, at).isLeft
        )
      },
      test("start succeeds after handicap applied") {
        val state = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Ready,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        assertTrue(Tournament.start(state, "wb-1-1", seq = 5, at).isRight)
      }
    ),
    suite("result")(
      test("MatchResult advances bracket and marks next matches ready") {
        val base = seededState()
        val state = withMatch(base, "wb-1-1") {
          _.copy(
            state = BracketMatchState.Started,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        val folded = for {
          event <- Tournament.recordResult(
            state,
            "wb-1-1",
            scoreA = 7,
            scoreB = 4,
            seq = 16,
            at
          )
          next <- Replay.replay(
            seededEvents(base) :+
              TournamentEvent.MatchReady(
                seq = 13,
                at = at,
                payload =
                  MatchReadyPayload(matchId = "wb-1-1", handicapSuggested = 2)
              ) :+
              TournamentEvent.HandicapApplied(
                seq = 14,
                at = at,
                payload = HandicapAppliedPayload(
                  matchId = "wb-1-1",
                  handicapApplied = 2
                )
              ) :+
              TournamentEvent.MatchStarted(
                seq = 15,
                at = at,
                payload = MatchStartedPayload(matchId = "wb-1-1")
              ) :+
              event
          )
        } yield next
        val after = folded.toOption.get
        assertTrue(
          folded.isRight,
          matchOf(after, "wb-1-1").state == BracketMatchState.Completed,
          matchOf(after, "wb-2-1").playerA.contains(Player("P1"))
        )
      },
      test("result rejects winner score below race-to") {
        val state = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Started,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        assertTrue(
          Tournament
            .recordResult(state, "wb-1-1", scoreA = 6, scoreB = 4, seq = 6, at)
            .isLeft
        )
      },
      test("result rejects winner score above race-to") {
        val state = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Started,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        assertTrue(
          Tournament
            .recordResult(
              state,
              "wb-1-1",
              scoreA = 999,
              scoreB = 0,
              seq = 6,
              at
            )
            .isLeft
        )
      },
      test("result rejects tied scores at race-to") {
        val state = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Started,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        assertTrue(
          Tournament
            .recordResult(state, "wb-1-1", scoreA = 7, scoreB = 7, seq = 6, at)
            .isLeft
        )
      },
      test("result rejects matches that have not started") {
        val state = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Ready,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        assertTrue(
          Tournament
            .recordResult(state, "wb-1-1", scoreA = 7, scoreB = 4, seq = 6, at)
            .isLeft
        )
      }
    ),
    suite("full lifecycle")(
      test("ready → handicap → start → result replays cleanly") {
        val state = seededState()
        val seeded = seededEvents(state)
        val ready = Tournament.ready(state, "wb-1-1", seq = 13, at).toOption.get
        val afterReady = Replay.replay(seeded :+ ready).toOption.get
        val handicap =
          Tournament
            .applyHandicap(afterReady, "wb-1-1", handicap = 3, seq = 14, at)
            .toOption
            .get
        val afterHandicap =
          Replay.replay(seeded :+ ready :+ handicap).toOption.get
        val started =
          Tournament.start(afterHandicap, "wb-1-1", seq = 15, at).toOption.get
        val afterStart =
          Replay.replay(seeded :+ ready :+ handicap :+ started).toOption.get
        val result = Tournament
          .recordResult(
            afterStart,
            "wb-1-1",
            scoreA = 7,
            scoreB = 4,
            seq = 16,
            at
          )
          .toOption
          .get
        val finalState =
          Replay
            .replay(seeded :+ ready :+ handicap :+ started :+ result)
            .toOption
            .get
        val matchDef = matchOf(finalState, "wb-1-1")
        assertTrue(
          matchDef.state == BracketMatchState.Completed,
          matchDef.handicapSuggested.contains(ready.payload.handicapSuggested),
          matchDef.handicapApplied.contains(3),
          matchDef.result.contains(MatchResult(7, 4))
        )
      },
      test("illegal transitions fail during replay") {
        val state = seededState()
        assertTrue(
          Replay
            .replay(
              seededEvents(state) :+
                TournamentEvent.HandicapApplied(
                  seq = 9,
                  at = at,
                  payload = HandicapAppliedPayload(
                    matchId = "wb-1-1",
                    handicapApplied = 2
                  )
                )
            )
            .isLeft,
          Replay
            .replay(
              seededEvents(state) :+
                TournamentEvent.MatchStarted(
                  seq = 9,
                  at = at,
                  payload = MatchStartedPayload(matchId = "wb-1-1")
                )
            )
            .isLeft
        )
      }
    ),
    suite("wizard")(
      test("create produces TournamentCreated with empty players") {
        val result = Tournament.create("Spring Open", seq = 1, at)
        assertTrue(
          result.isRight,
          result.toOption.get.payload.name == "Spring Open",
          result.toOption.get.payload.players.isEmpty
        )
      },
      test("create rejects blank tournament name") {
        assertTrue(Tournament.create("  ", seq = 1, at).isLeft)
      },
      test("setPlayers rejects duplicate names") {
        val state = TournamentState(name = "Open", players = Nil)
        val players = List(Player("Alice"), Player("Alice"))
        val result = Tournament.setPlayers(state, players, seq = 2, at)
        assertTrue(
          result.isLeft,
          result.left.toOption.get.message.contains("duplicate player names")
        )
      },
      test("setPlayers allows names that differ only by case") {
        val state = TournamentState(name = "Open", players = Nil)
        val players = List(Player("Alice"), Player("alice"))
        assertTrue(
          Tournament.setPlayers(state, players, seq = 2, at).isRight
        )
      },
      test("setPlayers rejects when roster is locked") {
        val state = TournamentState(
          name = "Open",
          players = (1 to 8).map(i => Player(s"P$i")).toList,
          playersLocked = true
        )
        assertTrue(
          Tournament
            .setPlayers(state, List(Player("Guest")), seq = 2, at)
            .isLeft
        )
      },
      test("setPlayers rejects after bracket is seeded") {
        val state = seededState()
        assertTrue(
          Tournament
            .setPlayers(state, List(Player("Guest")), seq = 3, at)
            .isLeft
        )
      },
      test("lockPlayers rejects invalid player count") {
        val state = TournamentState(
          name = "Open",
          players = List(Player("Alice"), Player("Bob"))
        )
        assertTrue(Tournament.lockPlayers(state, seq = 2, at).isLeft)
      },
      test("lockPlayers succeeds with 3 players") {
        val state = TournamentState(
          name = "Open",
          players = List(Player("Alice"), Player("Bob"), Player("Carol"))
        )
        assertTrue(Tournament.lockPlayers(state, seq = 2, at).isRight)
      },
      test("lockPlayers succeeds with 8 players") {
        val state = TournamentState(
          name = "Open",
          players = (1 to 8).map(i => Player(s"P$i")).toList
        )
        assertTrue(Tournament.lockPlayers(state, seq = 2, at).isRight)
      },
      test("setRaceToByScope rejects incomplete scope map") {
        val state = TournamentState(
          name = "Open",
          players = (1 to 8).map(i => Player(s"P$i")).toList,
          playersLocked = true
        )
        val incomplete = RaceToTestSupport.uniformRaceTo(8) - "gf"
        val result =
          Tournament.setRaceToByScope(state, incomplete, startSeq = 3, at)
        assertTrue(
          result.isLeft,
          result.left.toOption.get.message
            .contains("every scope is configured")
        )
      },
      test("setRaceToByScope rejects race-to below 2") {
        val state = TournamentState(
          name = "Open",
          players = (1 to 8).map(i => Player(s"P$i")).toList,
          playersLocked = true
        )
        val invalid =
          RaceToTestSupport.uniformRaceTo(8).updated("wb-1", 1)
        val result =
          Tournament.setRaceToByScope(state, invalid, startSeq = 3, at)
        assertTrue(
          result.isLeft,
          result.left.toOption.get.message
            .contains("race-to must be at least 2")
        )
      },
      test("setRaceToByScope emits one event per scope") {
        val state = TournamentState(
          name = "Open",
          players = (1 to 8).map(i => Player(s"P$i")).toList,
          playersLocked = true
        )
        val result =
          Tournament.setRaceToByScope(
            state,
            RaceToTestSupport.uniformRaceTo(8).updated("wb-2", 5),
            startSeq = 3,
            at
          )
        assertTrue(
          result.isRight,
          result.toOption.get.size == 8,
          result.toOption.get.map(_.payload.scope).contains("wb-2"),
          result.toOption.get
            .find(_.payload.scope == "wb-2")
            .exists(_.payload.raceTo == 5)
        )
      },
      test("setRaceToByScope rejects after bracket is seeded") {
        assertTrue(
          Tournament
            .setRaceToByScope(
              seededState(),
              RaceToTestSupport.uniformRaceTo(8),
              startSeq = 3,
              at
            )
            .isLeft
        )
      }
    ),
    suite("resolveRaceTo")(
      test("uses per-section values from raceToByScope") {
        val raceTo = RaceToTestSupport.differentiatedRaceTo(8)
        val state = seededState(raceTo)
        assertTrue(
          MatchLifecycle.resolveRaceTo(state, "wb-1-1") == Right(7),
          MatchLifecycle.resolveRaceTo(state, "lb-1-1") == Right(5),
          MatchLifecycle.resolveRaceTo(state, "gf-1") == Right(9)
        )
      }
    ),
    suite("result with differentiated race-to")(
      test("enforces losers-bracket race-to on result") {
        val state = withMatch(
          seededState(RaceToTestSupport.differentiatedRaceTo(8)),
          "lb-1-1"
        ) {
          _.copy(
            state = BracketMatchState.Started,
            handicapSuggested = Some(0),
            handicapApplied = Some(0)
          )
        }
        assertTrue(
          Tournament
            .recordResult(state, "lb-1-1", scoreA = 5, scoreB = 3, seq = 6, at)
            .isRight,
          Tournament
            .recordResult(state, "lb-1-1", scoreA = 7, scoreB = 3, seq = 6, at)
            .isLeft
        )
      }
    )
  )
}
