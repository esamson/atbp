package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.bracket.TournamentBounds
import ph.samson.atbp.liga.handicap.HandicapCap
import ph.samson.atbp.liga.model.*

/** Pure validators shared by command handlers and event replay. */
object TournamentValidation {

  def validatePlayerCount(count: Int): Either[String, Unit] =
    if (TournamentBounds.validPlayerCount(count)) {
      Right(())
    } else {
      Left(TournamentBounds.invalidPlayerCountMessage(count))
    }

  def validateRaceTo(raceTo: Int): Either[String, Unit] =
    if (raceTo < 2) {
      Left("race-to must be at least 2")
    } else {
      Right(())
    }

  def validatePlayersSet(players: List[Player]): Either[String, Unit] =
    if (players.distinct.size != players.size) {
      Left("roster contains duplicate player names")
    } else {
      Right(())
    }

  def validateSeedState(state: TournamentState): Either[String, Unit] =
    if (state.completed) {
      Left("tournament is already completed")
    } else if (state.bracket.nonEmpty) {
      Left("bracket is already seeded")
    } else if (!state.playersLocked) {
      Left("cannot seed bracket before roster is locked")
    } else if (state.players.isEmpty) {
      Left("tournament has no players")
    } else if (!TournamentPhase.raceToComplete(state)) {
      Left("cannot seed bracket before race-to is set for all scopes")
    } else {
      Right(())
    }

  def validateHandicap(
      state: TournamentState,
      matchId: String,
      handicap: Int
  ): Either[String, Unit] =
    for {
      _ <- MatchLifecycle.requireActive(state).left.map(_.message)
      matchDef <- MatchLifecycle.findMatch(state, matchId).left.map(_.message)
      _ <- MatchLifecycle.validateHandicap(matchDef).left.map(_.message)
      raceTo <- MatchLifecycle.resolveRaceTo(state, matchId).left.map(_.message)
      cap = HandicapCap.capFor(raceTo)
      _ <-
        if (handicap < 0) {
          Left("handicap must be non-negative")
        } else if (handicap > cap) {
          Left(s"handicap must be at most $cap for race-to $raceTo")
        } else {
          Right(())
        }
    } yield ()

  def validateMatchResult(
      state: TournamentState,
      matchDef: BracketMatch,
      scoreA: Int,
      scoreB: Int
  ): Either[String, Unit] =
    for {
      raceTo <- MatchLifecycle
        .resolveRaceTo(state, matchDef.id)
        .left
        .map(_.message)
      _ <-
        if (scoreA == scoreB) {
          Left("scores cannot tie")
        } else if (scoreA < 0 || scoreB < 0) {
          Left("scores must be non-negative")
        } else {
          val winnerScore = math.max(scoreA, scoreB)
          val loserScore = math.min(scoreA, scoreB)
          if (winnerScore != raceTo) {
            Left(s"winner score must be $raceTo")
          } else if (loserScore >= raceTo) {
            Left(s"loser score must be less than $raceTo")
          } else {
            Right(())
          }
        }
    } yield ()
}
