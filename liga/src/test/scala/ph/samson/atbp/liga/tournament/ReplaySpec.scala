package ph.samson.atbp.liga.tournament

import better.files.File
import ph.samson.atbp.liga.bracket.BracketGen
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.testsupport.RaceToTestSupport
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.*
import zio.test.*

import java.time.Instant

object ReplaySpec extends ZIOSpecDefault {

  private val at = Instant.parse("2026-03-15T18:00:00Z")

  private def rating(name: String, r: Double): PlayerRating =
    PlayerRating(Player(name), r, rd = 100, wins = 0, losses = 0)

  private def eightPlayerRatings: List[PlayerRating] =
    (1 to 8).map(i => rating(s"P$i", 1700 - i * 10)).toList

  private def withTempDir[R, A](
      f: File => ZIO[R, Throwable, A]
  ): ZIO[R, Throwable, A] =
    ZIO.acquireReleaseWith(
      ZIO.attemptBlocking(File.newTemporaryDirectory("liga-replay"))
    )(dir => ZIO.attemptBlocking(dir.delete()).ignore)(f)

  private def appendAll(dir: File, events: List[TournamentEvent]): Task[Unit] =
    ZIO.foreachDiscard(events)(event => EventLog.append(dir, event))

  private def seededSeqAfterLockedRoster(startSeq: Int): Int =
    startSeq + 2 + RaceToTestSupport.uniformRaceTo(8).size

  private def lockedRosterEvents(startSeq: Int): List[TournamentEvent] = {
    val players = (1 to 8).map(i => Player(s"P$i")).toList
    val raceToEvents =
      RaceToTestSupport.raceToSetEvents(
        playerCount = 8,
        startSeq = startSeq + 2,
        at = at
      )
    List(
      TournamentEvent.PlayersSet(
        seq = startSeq,
        at = at,
        payload = PlayersSetPayload(players = players)
      ),
      TournamentEvent.PlayersLocked(
        seq = startSeq + 1,
        at = at,
        payload = PlayersLockedPayload()
      )
    ) ++ raceToEvents
  }

  def spec = suite("Replay")(
    test("EventLog writes zero-padded filenames and never overwrites") {
      withTempDir { dir =>
        val created = TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload(
            name = "Spring Open",
            players = eightPlayerRatings.map(_.player)
          )
        )
        val seeded = TournamentEvent.BracketSeeded(
          seq = 2,
          at = at,
          payload = BracketSeededPayload(
            frozenRatings = eightPlayerRatings,
            bracket = BracketGen.generate(eightPlayerRatings)
          )
        )
        for {
          _ <- EventLog.append(dir, created)
          _ <- EventLog.append(dir, seeded)
          files = dir.list.toList.sortBy(_.name)
          loaded <- EventLog.read(dir)
          duplicate <- EventLog.append(dir, created).either
        } yield assertTrue(
          files
            .map(_.name) == List("000001-created.json", "000002-seeded.json"),
          loaded.size == 2,
          duplicate.isLeft
        )
      }
    },
    test("replay from fixture dir reproduces bracket after seeding") {
      val dir = File(getClass.getResource("/tournaments/eight-player-seeded"))
      for {
        state <- Replay.replayDir(dir)
      } yield assertTrue(
        state.name == "Spring Open",
        state.players.size == 8,
        state.frozenRatings.size == 8,
        state.bracket.exists(_.size == 8),
        !Replay.isComplete(state),
        state.bracket.exists(_.matches.exists(_.id == "wb-1-1"))
      )
    },
    test("replay folds match lifecycle events onto bracket") {
      withTempDir { dir =>
        val bracket = BracketGen.generate(eightPlayerRatings)
        val events = List(
          TournamentEvent.Created(
            seq = 1,
            at = at,
            payload = TournamentCreatedPayload(
              name = "Spring Open",
              players = Nil
            )
          )
        ) ++ lockedRosterEvents(startSeq = 2) ++ List(
          TournamentEvent.BracketSeeded(
            seq = seededSeqAfterLockedRoster(startSeq = 2),
            at = at,
            payload = BracketSeededPayload(
              frozenRatings = eightPlayerRatings,
              bracket = bracket
            )
          ),
          TournamentEvent.MatchReady(
            seq = seededSeqAfterLockedRoster(startSeq = 2) + 1,
            at = at,
            payload =
              MatchReadyPayload(matchId = "wb-1-1", handicapSuggested = 2)
          ),
          TournamentEvent.HandicapApplied(
            seq = seededSeqAfterLockedRoster(startSeq = 2) + 2,
            at = at,
            payload =
              HandicapAppliedPayload(matchId = "wb-1-1", handicapApplied = 3)
          ),
          TournamentEvent.MatchStarted(
            seq = seededSeqAfterLockedRoster(startSeq = 2) + 3,
            at = at,
            payload = MatchStartedPayload(matchId = "wb-1-1")
          ),
          TournamentEvent.MatchResult(
            seq = seededSeqAfterLockedRoster(startSeq = 2) + 4,
            at = at,
            payload =
              MatchResultPayload(matchId = "wb-1-1", scoreA = 7, scoreB = 4)
          )
        )
        for {
          _ <- appendAll(dir, events)
          state <- Replay.replayDir(dir)
          matchDef = state.bracket.flatMap(_.matches.find(_.id == "wb-1-1")).get
        } yield assertTrue(
          matchDef.state == BracketMatchState.Completed,
          matchDef.handicapSuggested.contains(2),
          matchDef.handicapApplied.contains(3),
          matchDef.result.contains(MatchResult(7, 4))
        )
      }
    },
    test("partial replay without TournamentCompleted is incomplete") {
      val dir = File(getClass.getResource("/tournaments/eight-player-partial"))
      for {
        state <- Replay.replayDir(dir)
      } yield assertTrue(
        !Replay.isComplete(state),
        state.bracket.exists(_.matches.exists(_.id == "wb-1-1")),
        state.bracket
          .flatMap(_.matches.find(_.id == "wb-1-1"))
          .exists(_.state == BracketMatchState.Started)
      )
    },
    test("replay marks complete when TournamentCompleted is present") {
      val dir = File(getClass.getResource("/tournaments/eight-player-complete"))
      for {
        state <- Replay.replayDir(dir)
      } yield assertTrue(
        Replay.isComplete(state),
        state.completed
      )
    },
    test("replay rejects non-monotonic seq values") {
      val events = List(
        TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload("Open", Nil)
        ),
        TournamentEvent.MatchStarted(
          seq = 1,
          at = at,
          payload = MatchStartedPayload("wb-1-1")
        )
      )
      assertTrue(Replay.replay(events).isLeft)
    },
    test("EventLog read sorts by seq not filename") {
      withTempDir { dir =>
        val first = TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload(
            name = "Spring Open",
            players = eightPlayerRatings.map(_.player)
          )
        )
        val second = TournamentEvent.BracketSeeded(
          seq = 2,
          at = at,
          payload = BracketSeededPayload(
            frozenRatings = eightPlayerRatings,
            bracket = BracketGen.generate(eightPlayerRatings)
          )
        )
        for {
          _ <- ZIO.attemptBlocking {
            dir
              .createChild("000002-seeded.json")
              .write(EventCodec.encode(second))
            dir
              .createChild("000001-created.json")
              .write(EventCodec.encode(first))
          }
          loaded <- EventLog.read(dir)
        } yield assertTrue(loaded.map(_.seq) == List(1, 2))
      }
    },
    test("PlayersSet replaces roster before lock") {
      val events = List(
        TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload("Open", Nil)
        ),
        TournamentEvent.PlayersSet(
          seq = 2,
          at = at,
          payload = PlayersSetPayload(
            players = (1 to 8).map(i => Player(s"P$i")).toList
          )
        )
      )
      for {
        state <- ZIO.fromEither(Replay.replay(events))
      } yield assertTrue(
        state.players.size == 8,
        !state.playersLocked,
        TournamentPhase.derive(state, hasDir = true) == TournamentPhase.Defining
      )
    },
    test("PlayersLocked sets playersLocked for 3 players") {
      val players = List(Player("P1"), Player("P2"), Player("P3"))
      val events = List(
        TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload("Open", Nil)
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
      )
      for {
        state <- ZIO.fromEither(Replay.replay(events))
      } yield assertTrue(
        state.playersLocked,
        state.players.size == 3,
        TournamentPhase.derive(state, hasDir = true) == TournamentPhase.Locked
      )
    },
    test("PlayersLocked sets playersLocked when count is valid") {
      val players = (1 to 8).map(i => Player(s"P$i")).toList
      val events = List(
        TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload("Open", Nil)
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
      )
      for {
        state <- ZIO.fromEither(Replay.replay(events))
      } yield assertTrue(
        state.playersLocked,
        TournamentPhase.derive(state, hasDir = true) == TournamentPhase.Locked
      )
    },
    test("PlayersSet after lock is rejected") {
      val players = (1 to 8).map(i => Player(s"P$i")).toList
      val events = List(
        TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload("Open", Nil)
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
        ),
        TournamentEvent.PlayersSet(
          seq = 4,
          at = at,
          payload = PlayersSetPayload(players = Player("Guest") :: players)
        )
      )
      assertTrue(Replay.replay(events).isLeft)
    },
    test("PlayersLocked rejects invalid player count") {
      val events = List(
        TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload("Open", Nil)
        ),
        TournamentEvent.PlayersSet(
          seq = 2,
          at = at,
          payload = PlayersSetPayload(
            players = List(Player("Alice"), Player("Bob"))
          )
        ),
        TournamentEvent.PlayersLocked(
          seq = 3,
          at = at,
          payload = PlayersLockedPayload()
        )
      )
      assertTrue(Replay.replay(events).isLeft)
    },
    test("phase is raceTo when all required round keys are set") {
      val players = (1 to 8).map(i => Player(s"P$i")).toList
      val events = List(
        TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload("Open", Nil)
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
      ) ++ RaceToTestSupport.raceToSetEvents(
        playerCount = 8,
        startSeq = 4,
        at = at
      )
      for {
        state <- ZIO.fromEither(Replay.replay(events))
      } yield assertTrue(
        TournamentPhase.derive(state, hasDir = true) == TournamentPhase.RaceTo,
        TournamentPhase.raceToComplete(state)
      )
    },
    test("phase is none when no tournament dir exists") {
      val state = TournamentState(name = "", players = Nil)
      assertTrue(
        TournamentPhase.derive(state, hasDir = false) == TournamentPhase.None
      )
    },
    test("replay rejects duplicate player names on PlayersSet from disk") {
      withTempDir { dir =>
        val events = List(
          TournamentEvent.Created(
            seq = 1,
            at = at,
            payload = TournamentCreatedPayload("Open", Nil)
          ),
          TournamentEvent.PlayersSet(
            seq = 2,
            at = at,
            payload = PlayersSetPayload(
              players = List(Player("Alice"), Player("Alice"))
            )
          )
        )
        for {
          _ <- appendAll(dir, events)
          result <- Replay.replayDir(dir).either
        } yield assertTrue(
          result.isLeft,
          result.left.exists(_.getMessage.contains("duplicate"))
        )
      }
    },
    test("replay rejects BracketSeeded before roster lock from disk") {
      withTempDir { dir =>
        val events = List(
          TournamentEvent.Created(
            seq = 1,
            at = at,
            payload = TournamentCreatedPayload("Open", Nil)
          ),
          TournamentEvent.PlayersSet(
            seq = 2,
            at = at,
            payload = PlayersSetPayload(
              players = (1 to 8).map(i => Player(s"P$i")).toList
            )
          ),
          TournamentEvent.BracketSeeded(
            seq = 3,
            at = at,
            payload = BracketSeededPayload(
              frozenRatings = eightPlayerRatings,
              bracket = BracketGen.generate(eightPlayerRatings)
            )
          )
        )
        for {
          _ <- appendAll(dir, events)
          result <- Replay.replayDir(dir).either
        } yield assertTrue(
          result.isLeft,
          result.left.exists(_.getMessage.contains("locked"))
        )
      }
    },
    test("replay rejects invalid handicap from disk") {
      withTempDir { dir =>
        val bracket = BracketGen.generate(eightPlayerRatings)
        val events = List(
          TournamentEvent.Created(
            seq = 1,
            at = at,
            payload = TournamentCreatedPayload("Open", Nil)
          )
        ) ++ lockedRosterEvents(startSeq = 2) ++ List(
          TournamentEvent.BracketSeeded(
            seq = seededSeqAfterLockedRoster(startSeq = 2),
            at = at,
            payload = BracketSeededPayload(
              frozenRatings = eightPlayerRatings,
              bracket = bracket
            )
          ),
          TournamentEvent.MatchReady(
            seq = seededSeqAfterLockedRoster(startSeq = 2) + 1,
            at = at,
            payload =
              MatchReadyPayload(matchId = "wb-1-1", handicapSuggested = 2)
          ),
          TournamentEvent.HandicapApplied(
            seq = seededSeqAfterLockedRoster(startSeq = 2) + 2,
            at = at,
            payload =
              HandicapAppliedPayload(matchId = "wb-1-1", handicapApplied = 99)
          )
        )
        for {
          _ <- appendAll(dir, events)
          result <- Replay.replayDir(dir).either
        } yield assertTrue(
          result.isLeft,
          result.left.exists(_.getMessage.contains("handicap"))
        )
      }
    },
    test("replay rejects invalid match scores from disk") {
      withTempDir { dir =>
        val bracket = BracketGen.generate(eightPlayerRatings)
        val events = List(
          TournamentEvent.Created(
            seq = 1,
            at = at,
            payload = TournamentCreatedPayload("Open", Nil)
          )
        ) ++ lockedRosterEvents(startSeq = 2) ++ List(
          TournamentEvent.BracketSeeded(
            seq = seededSeqAfterLockedRoster(startSeq = 2),
            at = at,
            payload = BracketSeededPayload(
              frozenRatings = eightPlayerRatings,
              bracket = bracket
            )
          ),
          TournamentEvent.MatchReady(
            seq = seededSeqAfterLockedRoster(startSeq = 2) + 1,
            at = at,
            payload =
              MatchReadyPayload(matchId = "wb-1-1", handicapSuggested = 2)
          ),
          TournamentEvent.HandicapApplied(
            seq = seededSeqAfterLockedRoster(startSeq = 2) + 2,
            at = at,
            payload =
              HandicapAppliedPayload(matchId = "wb-1-1", handicapApplied = 0)
          ),
          TournamentEvent.MatchStarted(
            seq = seededSeqAfterLockedRoster(startSeq = 2) + 3,
            at = at,
            payload = MatchStartedPayload(matchId = "wb-1-1")
          ),
          TournamentEvent.MatchResult(
            seq = seededSeqAfterLockedRoster(startSeq = 2) + 4,
            at = at,
            payload =
              MatchResultPayload(matchId = "wb-1-1", scoreA = 5, scoreB = 4)
          )
        )
        for {
          _ <- appendAll(dir, events)
          result <- Replay.replayDir(dir).either
        } yield assertTrue(
          result.isLeft,
          result.left.exists(_.getMessage.contains("winner score must be 7"))
        )
      }
    },
    test("replay rejects invalid race-to from disk") {
      withTempDir { dir =>
        val events = List(
          TournamentEvent.Created(
            seq = 1,
            at = at,
            payload = TournamentCreatedPayload("Open", Nil)
          ),
          TournamentEvent.RaceToSet(
            seq = 2,
            at = at,
            payload = RaceToSetPayload(scope = "wb-1", raceTo = 1)
          )
        )
        for {
          _ <- appendAll(dir, events)
          result <- Replay.replayDir(dir).either
        } yield assertTrue(
          result.isLeft,
          result.left.exists(
            _.getMessage.contains("race-to must be at least 2")
          )
        )
      }
    }
  )
}
