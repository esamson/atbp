package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.bracket.BracketGen
import ph.samson.atbp.liga.bracket.RaceToScopes
import ph.samson.atbp.liga.bracket.TournamentBounds
import ph.samson.atbp.liga.glicko.Tuning
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent

import java.time.Instant

/** Builds bracket seeding events from period-end ratings. */
object Seed {

  sealed trait Error {
    def message: String
  }

  final case class TournamentCompletedError() extends Error {
    val message: String = "tournament is already completed"
  }

  final case class AlreadySeededError() extends Error {
    val message: String = "bracket is already seeded"
  }

  final case class NoPlayersError() extends Error {
    val message: String = "tournament has no players"
  }

  final case class InvalidPlayerCountError(count: Int) extends Error {
    val message: String = TournamentBounds.invalidPlayerCountMessage(count)
  }

  final case class MissingPlayerError(name: String) extends Error {
    val message: String = s"unknown player: $name"
  }

  final case class PlayersNotLockedError() extends Error {
    val message: String = "cannot seed bracket before roster is locked"
  }

  final case class RaceToIncompleteError() extends Error {
    val message: String =
      "cannot seed bracket before race-to is set for all scopes"
  }

  final case class InvalidRaceToError(raceTo: Int) extends Error {
    val message: String = s"race-to must be at least 2: $raceTo"
  }

  def buildEvents(
      state: TournamentState,
      periodRatings: List[PlayerRating],
      raceToByScope: Map[String, Int],
      startSeq: Int,
      at: Instant
  ): Either[Error, List[TournamentEvent]] =
    for {
      _ <- validateState(state)
      effectiveRaceTo =
        if (raceToByScope.isEmpty) {
          state.raceToByScope
        } else {
          raceToByScope
        }
      _ <- validateRaceToByScope(effectiveRaceTo, state.players.size)
      ratings <- resolveRatings(state.players, periodRatings)
      _ <- validatePlayerCount(ratings.size)
      bracket = BracketGen.generate(ratings)
      raceToAlreadySaved =
        raceToByScope.isEmpty && TournamentPhase.raceToComplete(state)
      raceToEvents =
        if (raceToAlreadySaved) {
          Nil
        } else {
          RaceToScopes
            .requiredKeys(state.players.size)
            .zipWithIndex
            .map { case (scope, index) =>
              TournamentEvent.RaceToSet(
                seq = startSeq + index,
                at = at,
                payload = RaceToSetPayload(
                  scope = scope,
                  raceTo = effectiveRaceTo(scope)
                )
              )
            }
        }
      seededSeq = startSeq + raceToEvents.size
      seeded = TournamentEvent.BracketSeeded(
        seq = seededSeq,
        at = at,
        payload = BracketSeededPayload(
          frozenRatings = ratings,
          bracket = bracket
        )
      )
    } yield raceToEvents :+ seeded

  private def validateState(
      state: TournamentState
  ): Either[Error, Unit] =
    TournamentValidation.validateSeedState(state).left.map {
      case "tournament is already completed" => TournamentCompletedError()
      case "bracket is already seeded"       => AlreadySeededError()
      case "cannot seed bracket before roster is locked" =>
        PlayersNotLockedError()
      case "tournament has no players" => NoPlayersError()
      case "cannot seed bracket before race-to is set for all scopes" =>
        RaceToIncompleteError()
      case _ => NoPlayersError()
    }

  private def validateRaceToByScope(
      raceToByScope: Map[String, Int],
      playerCount: Int
  ): Either[Error, Unit] = {
    val required = RaceToScopes.requiredKeys(playerCount)
    val missing = required.filterNot(raceToByScope.contains)
    if (missing.nonEmpty) {
      Left(RaceToIncompleteError())
    } else {
      raceToByScope.values.toList.foldLeft(Right(()): Either[Error, Unit]) {
        case (Right(_), raceTo) =>
          TournamentValidation
            .validateRaceTo(raceTo)
            .left
            .map(_ => InvalidRaceToError(raceTo))
        case (left, _) => left
      }
    }
  }

  private def validatePlayerCount(count: Int): Either[Error, Unit] =
    TournamentValidation.validatePlayerCount(count).left.map { _ =>
      InvalidPlayerCountError(count)
    }

  private def resolveRatings(
      players: List[Player],
      periodRatings: List[PlayerRating]
  ): Either[Error, List[PlayerRating]] = {
    val byName = periodRatings.map(r => r.player.name -> r).toMap
    val tuning = Tuning.Default
    players.foldLeft(Right(Nil): Either[Error, List[PlayerRating]]) {
      case (Right(acc), player) =>
        val rating = byName.getOrElse(
          player.name,
          PlayerRating(
            player = player,
            rating = tuning.initRating,
            rd = tuning.maxDeviation,
            wins = 0,
            losses = 0
          )
        )
        Right(acc :+ rating)
      case (left, _) => left
    }
  }
}
