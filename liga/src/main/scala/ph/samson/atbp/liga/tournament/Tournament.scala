package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.bracket.RaceToScopes
import ph.samson.atbp.liga.bracket.TournamentBounds
import ph.samson.atbp.liga.handicap.Handicap
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent

import java.time.Instant
import java.time.LocalDate

/** Pure tournament command handlers that validate lifecycle rules and produce
  * events.
  */
object Tournament {

  type Error = MatchLifecycle.Error

  sealed trait WizardError {
    def message: String
  }

  final case class RosterLockedError() extends WizardError {
    val message: String = "cannot set players after roster is locked"
  }

  final case class AlreadySeededError() extends WizardError {
    val message: String = "cannot modify tournament after bracket is seeded"
  }

  final case class RosterAlreadyLockedError() extends WizardError {
    val message: String = "roster is already locked"
  }

  final case class NoPlayersError() extends WizardError {
    val message: String = "cannot lock roster with no players"
  }

  final case class InvalidPlayerCountError(count: Int) extends WizardError {
    val message: String = TournamentBounds.invalidPlayerCountMessage(count)
  }

  final case class EmptyTournamentNameError() extends WizardError {
    val message: String =
      "tournament name must contain at least one letter or digit"
  }

  final case class DuplicatePlayersError() extends WizardError {
    val message: String = "roster contains duplicate player names"
  }

  final case class InvalidRaceToError(raceTo: Int) extends WizardError {
    val message: String = s"race-to must be at least 2: $raceTo"
  }

  final case class RaceToIncompleteError() extends WizardError {
    val message: String =
      "cannot save race-to before every scope is configured"
  }

  def create(
      name: String,
      seq: Int,
      at: Instant
  ): Either[WizardError, TournamentEvent.Created] =
    if (Resume.slugify(name).isEmpty) {
      Left(EmptyTournamentNameError())
    } else {
      Right(
        TournamentEvent.Created(
          seq = seq,
          at = at,
          payload = TournamentCreatedPayload(name = name.trim, players = Nil)
        )
      )
    }

  def setPlayers(
      state: TournamentState,
      players: List[Player],
      seq: Int,
      at: Instant
  ): Either[WizardError, TournamentEvent.PlayersSet] =
    if (state.playersLocked) {
      Left(RosterLockedError())
    } else if (state.bracket.nonEmpty) {
      Left(AlreadySeededError())
    } else {
      TournamentValidation
        .validatePlayersSet(players)
        .fold(
          _ => Left(DuplicatePlayersError()),
          _ =>
            Right(
              TournamentEvent.PlayersSet(
                seq = seq,
                at = at,
                payload = PlayersSetPayload(players = players)
              )
            )
        )
    }

  def lockPlayers(
      state: TournamentState,
      seq: Int,
      at: Instant
  ): Either[WizardError, TournamentEvent.PlayersLocked] =
    if (state.playersLocked) {
      Left(RosterAlreadyLockedError())
    } else if (state.players.isEmpty) {
      Left(NoPlayersError())
    } else if (!TournamentBounds.validPlayerCount(state.players.size)) {
      Left(InvalidPlayerCountError(state.players.size))
    } else {
      Right(
        TournamentEvent.PlayersLocked(
          seq = seq,
          at = at,
          payload = PlayersLockedPayload()
        )
      )
    }

  def setRaceToByScope(
      state: TournamentState,
      raceToByScope: Map[String, Int],
      startSeq: Int,
      at: Instant
  ): Either[WizardError, List[TournamentEvent.RaceToSet]] =
    if (state.bracket.nonEmpty) {
      Left(AlreadySeededError())
    } else {
      val required = RaceToScopes.requiredKeys(state.players.size)
      val missing = required.filterNot(raceToByScope.contains)
      if (missing.nonEmpty) {
        Left(RaceToIncompleteError())
      } else {
        required.zipWithIndex
          .foldLeft(
            Right(Nil): Either[WizardError, List[TournamentEvent.RaceToSet]]
          ) { case (acc, (scope, index)) =>
            acc.flatMap { events =>
              val raceTo = raceToByScope(scope)
              TournamentValidation
                .validateRaceTo(raceTo)
                .left
                .map(_ => InvalidRaceToError(raceTo))
                .map(_ =>
                  events :+ TournamentEvent.RaceToSet(
                    seq = startSeq + index,
                    at = at,
                    payload = RaceToSetPayload(scope = scope, raceTo = raceTo)
                  )
                )
            }
          }
      }
    }

  def ready(
      state: TournamentState,
      matchId: String,
      seq: Int,
      at: Instant
  ): Either[Error, TournamentEvent.MatchReady] =
    for {
      matchDef <- MatchLifecycle.findMatch(state, matchId)
      _ <- MatchLifecycle.validateReady(state, matchDef)
      raceTo <- MatchLifecycle.resolveRaceTo(state, matchId)
      ratingA <- frozenRating(state, matchDef.playerA.get)
      ratingB <- frozenRating(state, matchDef.playerB.get)
      suggestion = Handicap.suggest(ratingA, ratingB, raceTo)
    } yield TournamentEvent.MatchReady(
      seq = seq,
      at = at,
      payload = MatchReadyPayload(
        matchId = matchId,
        handicapSuggested = suggestion.handicap
      )
    )

  def applyHandicap(
      state: TournamentState,
      matchId: String,
      handicap: Int,
      seq: Int,
      at: Instant
  ): Either[Error, TournamentEvent.HandicapApplied] =
    for {
      _ <- TournamentValidation
        .validateHandicap(state, matchId, handicap)
        .left
        .map(msg =>
          MatchLifecycle.InvalidTransitionError(matchId, "handicap", msg)
        )
    } yield TournamentEvent.HandicapApplied(
      seq = seq,
      at = at,
      payload = HandicapAppliedPayload(
        matchId = matchId,
        handicapApplied = handicap
      )
    )

  def start(
      state: TournamentState,
      matchId: String,
      seq: Int,
      at: Instant
  ): Either[Error, TournamentEvent.MatchStarted] =
    for {
      _ <- MatchLifecycle.requireActive(state)
      matchDef <- MatchLifecycle.findMatch(state, matchId)
      _ <- MatchLifecycle.validateStart(matchDef)
    } yield TournamentEvent.MatchStarted(
      seq = seq,
      at = at,
      payload = MatchStartedPayload(matchId = matchId)
    )

  def recordResult(
      state: TournamentState,
      matchId: String,
      scoreA: Int,
      scoreB: Int,
      seq: Int,
      at: Instant
  ): Either[Error, TournamentEvent.MatchResult] =
    for {
      _ <- MatchLifecycle.requireActive(state)
      matchDef <- MatchLifecycle.findMatch(state, matchId)
      _ <- MatchLifecycle.validateResult(matchDef)
      _ <- TournamentValidation
        .validateMatchResult(state, matchDef, scoreA, scoreB)
        .left
        .map(msg =>
          MatchLifecycle.InvalidTransitionError(matchDef.id, "result", msg)
        )
    } yield TournamentEvent.MatchResult(
      seq = seq,
      at = at,
      payload = MatchResultPayload(
        matchId = matchId,
        scoreA = scoreA,
        scoreB = scoreB
      )
    )

  def complete(
      state: TournamentState,
      completed: LocalDate,
      seq: Int,
      at: Instant
  ): Either[Error, TournamentEvent.TournamentCompleted] =
    for {
      _ <- MatchLifecycle.requireActive(state)
      _ <- PeriodEmission
        .toPeriod(state, completed)
        .left
        .map(message =>
          MatchLifecycle.InvalidTransitionError("", "complete", message)
        )
    } yield TournamentEvent.TournamentCompleted(
      seq = seq,
      at = at,
      payload = TournamentCompletedPayload(completed = completed)
    )

  private def frozenRating(
      state: TournamentState,
      player: Player
  ): Either[Error, PlayerRating] =
    state.frozenRatings
      .get(player)
      .toRight(MatchLifecycle.MissingRatingError(player))
}
