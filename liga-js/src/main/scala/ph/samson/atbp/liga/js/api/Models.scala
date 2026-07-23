package ph.samson.atbp.liga.js.api

import zio.json.*

/** JSON shapes mirrored from `liga.serve.ApiJson` and `DirectorRoutes`. */
object Models {

  final case class Player(name: String)

  final case class PlayerRating(
      player: Player,
      rating: Double,
      rd: Double,
      wins: Int,
      losses: Int
  )

  final case class MatchResult(
      scoreA: Int,
      scoreB: Int
  )

  enum BracketMatchState {
    case Pending, Ready, Started, Completed
  }

  final case class BracketMatch(
      id: String,
      playerA: Option[Player],
      playerB: Option[Player],
      state: BracketMatchState,
      raceTo: Option[Int] = None,
      handicapSuggested: Option[Int] = None,
      handicapApplied: Option[Int] = None,
      result: Option[MatchResult] = None,
      isBye: Boolean = false
  )

  final case class Bracket(
      size: Int,
      matches: List[BracketMatch]
  )

  enum TournamentPhase {
    case None, Defining, Locked, RaceTo, Active, Completed
  }

  object TournamentPhase {
    def fromApi(value: String): TournamentPhase =
      value match {
        case "none"      => TournamentPhase.None
        case "defining"  => TournamentPhase.Defining
        case "locked"    => TournamentPhase.Locked
        case "raceTo"    => TournamentPhase.RaceTo
        case "active"    => TournamentPhase.Active
        case "completed" => TournamentPhase.Completed
        case _           => TournamentPhase.None
      }
  }

  final case class TournamentResponse(
      name: String,
      players: List[Player],
      completed: Boolean,
      phase: String,
      raceToByScope: Map[String, Int],
      bracket: Option[Bracket],
      frozenRatings: List[PlayerRating]
  )

  final case class LeaderboardResponse(
      ratings: List[PlayerRating]
  )

  final case class ConfigResponse(
      audiencePollIntervalSeconds: Int
  )

  final case class SeedRequest(raceToByScope: Map[String, Int])

  final case class CreateRequest(name: String)

  final case class PlayersRequest(players: List[Player])

  final case class RaceToRequest(raceToByScope: Map[String, Int])

  final case class HandicapRequest(handicap: Int)

  final case class ResultRequest(scoreA: Int, scoreB: Int)

  final case class CompleteRequest(completed: Option[String] = None)

  final case class HandicapSuggestion(
      weakerPlayer: Player,
      handicap: Int,
      raceTo: Int
  )

  given JsonCodec[Player] = DeriveJsonCodec.gen
  given JsonCodec[PlayerRating] = DeriveJsonCodec.gen
  given JsonCodec[MatchResult] = DeriveJsonCodec.gen
  given JsonCodec[BracketMatchState] = DeriveJsonCodec.gen
  given JsonCodec[BracketMatch] = DeriveJsonCodec.gen
  given JsonCodec[Bracket] = DeriveJsonCodec.gen
  given JsonCodec[TournamentResponse] = DeriveJsonCodec.gen
  given JsonCodec[LeaderboardResponse] = DeriveJsonCodec.gen
  given JsonCodec[ConfigResponse] = DeriveJsonCodec.gen
  given JsonEncoder[SeedRequest] = DeriveJsonEncoder.gen
  given JsonEncoder[CreateRequest] = DeriveJsonEncoder.gen
  given JsonEncoder[PlayersRequest] = DeriveJsonEncoder.gen
  given JsonEncoder[RaceToRequest] = DeriveJsonEncoder.gen
  given JsonEncoder[HandicapRequest] = DeriveJsonEncoder.gen
  given JsonEncoder[ResultRequest] = DeriveJsonEncoder.gen
  given JsonEncoder[CompleteRequest] = DeriveJsonEncoder.gen
}
