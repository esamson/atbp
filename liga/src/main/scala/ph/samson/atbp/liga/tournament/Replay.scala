package ph.samson.atbp.liga.tournament

import better.files.File
import ph.samson.atbp.liga.bracket.Advancement
import ph.samson.atbp.liga.bracket.TournamentBounds
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.Task
import zio.ZIO

/** Fold append-only tournament events into replay state. */
object Replay {

  final case class ReplayError(message: String) extends Exception(message)

  def replayDir(dir: File): Task[TournamentState] =
    EventLog.read(dir).flatMap { events =>
      ZIO.fromEither(replay(events).left.map(ReplayError(_)))
    }

  def replay(events: List[TournamentEvent]): Either[String, TournamentState] =
    for {
      _ <- EventCodec.validateMonotonicSeq(events)
      state <- events
        .sortBy(_.seq)
        .foldLeft(Right(empty): Either[String, TournamentState]) {
          case (Right(current), event) => applyEvent(current, event)
          case (left, _)               => left
        }
    } yield state

  def isComplete(state: TournamentState): Boolean =
    state.completed

  private val empty: TournamentState =
    TournamentState(name = "", players = Nil)

  private def applyEvent(
      state: TournamentState,
      event: TournamentEvent
  ): Either[String, TournamentState] =
    event match {
      case TournamentEvent.Created(_, _, payload) =>
        Right(
          state.copy(
            name = payload.name,
            players = payload.players
          )
        )

      case TournamentEvent.PlayersSet(_, _, payload) =>
        if (state.playersLocked) {
          Left("cannot set players after roster is locked")
        } else if (state.bracket.nonEmpty) {
          Left("cannot set players after bracket is seeded")
        } else {
          TournamentValidation.validatePlayersSet(payload.players).map { _ =>
            state.copy(players = payload.players)
          }
        }

      case TournamentEvent.PlayersLocked(_, _, _) =>
        if (state.playersLocked) {
          Left("roster is already locked")
        } else if (state.players.isEmpty) {
          Left("cannot lock roster with no players")
        } else if (!TournamentBounds.validPlayerCount(state.players.size)) {
          Left(TournamentBounds.invalidPlayerCountMessage(state.players.size))
        } else {
          Right(state.copy(playersLocked = true))
        }

      case TournamentEvent.RaceToSet(_, _, payload) =>
        for {
          _ <- TournamentValidation.validateRaceTo(payload.raceTo)
        } yield state.copy(
          raceToByScope =
            state.raceToByScope.updated(payload.scope, payload.raceTo)
        )

      case TournamentEvent.BracketSeeded(_, _, payload) =>
        for {
          _ <- TournamentValidation.validateSeedState(state)
        } yield state.copy(
          frozenRatings = payload.frozenRatings.map(r => r.player -> r).toMap,
          bracket = Some(payload.bracket)
        )

      case TournamentEvent.MatchReady(_, _, payload) =>
        for {
          matchDef <- findMatch(state, payload.matchId)
          _ <- MatchLifecycle.validateReady(state, matchDef).left.map(_.message)
          updated <- updateMatch(state, payload.matchId) { current =>
            current.copy(handicapSuggested = Some(payload.handicapSuggested))
          }
        } yield updated

      case TournamentEvent.HandicapApplied(_, _, payload) =>
        for {
          _ <- TournamentValidation.validateHandicap(
            state,
            payload.matchId,
            payload.handicapApplied
          )
          updated <- updateMatch(state, payload.matchId) { current =>
            current.copy(handicapApplied = Some(payload.handicapApplied))
          }
        } yield updated

      case TournamentEvent.MatchStarted(_, _, payload) =>
        for {
          _ <- MatchLifecycle.requireActive(state).left.map(_.message)
          matchDef <- findMatch(state, payload.matchId)
          _ <- MatchLifecycle.validateStart(matchDef).left.map(_.message)
          updated <- updateMatch(state, payload.matchId) { current =>
            current.copy(state = BracketMatchState.Started)
          }
        } yield updated

      case TournamentEvent.MatchResult(_, _, payload) =>
        for {
          _ <- MatchLifecycle.requireActive(state).left.map(_.message)
          matchDef <- findMatch(state, payload.matchId)
          _ <- MatchLifecycle.validateResult(matchDef).left.map(_.message)
          _ <- TournamentValidation.validateMatchResult(
            state,
            matchDef,
            payload.scoreA,
            payload.scoreB
          )
          result <- applyMatchResult(state, payload)
        } yield result

      case TournamentEvent.TournamentCompleted(_, _, _) =>
        Right(state.copy(completed = true))
    }

  private def findMatch(
      state: TournamentState,
      matchId: String
  ): Either[String, BracketMatch] =
    MatchLifecycle.findMatch(state, matchId).left.map(_.message)

  private def updateMatch(
      state: TournamentState,
      matchId: String
  )(
      update: BracketMatch => BracketMatch
  ): Either[String, TournamentState] =
    for {
      _ <- findMatch(state, matchId).map(_ => ())
      bracket <- state.bracket.toRight(s"no bracket loaded for match $matchId")
      updatedMatches = bracket.matches.map { matchDef =>
        if (matchDef.id == matchId) {
          update(matchDef)
        } else {
          matchDef
        }
      }
    } yield state.copy(bracket = Some(bracket.copy(matches = updatedMatches)))

  private def applyMatchResult(
      state: TournamentState,
      payload: MatchResultPayload
  ): Either[String, TournamentState] =
    for {
      bracket <- state.bracket.toRight("no bracket loaded for match result")
      matchDef <- bracket.matches
        .find(_.id == payload.matchId)
        .toRight(s"unknown match: ${payload.matchId}")
      winner <- winnerFromScores(matchDef, payload.scoreA, payload.scoreB)
      advanced <- Advancement.advance(bracket, payload.matchId, winner)
      withScores = advanced.bracket.copy(
        matches = advanced.bracket.matches.map { current =>
          if (current.id == payload.matchId) {
            current.copy(
              state = BracketMatchState.Completed,
              result = Some(MatchResult(payload.scoreA, payload.scoreB))
            )
          } else {
            current
          }
        }
      )
    } yield state.copy(bracket = Some(withScores))

  private def winnerFromScores(
      matchDef: BracketMatch,
      scoreA: Int,
      scoreB: Int
  ): Either[String, Player] =
    if (scoreA == scoreB) {
      Left(s"tie score in ${matchDef.id}")
    } else if (scoreA > scoreB) {
      matchDef.playerA.toRight(s"no player A in ${matchDef.id}")
    } else {
      matchDef.playerB.toRight(s"no player B in ${matchDef.id}")
    }
}
