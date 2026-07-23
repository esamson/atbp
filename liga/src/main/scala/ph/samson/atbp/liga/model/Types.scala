package ph.samson.atbp.liga.model

import java.time.LocalDate

/** Final scores for a completed match. */
final case class MatchResult(
    scoreA: Int,
    scoreB: Int
)

/** One completed period (typically one tournament) from a `*.liga` file. */
final case class Period(
    name: String,
    completed: LocalDate,
    format: Option[String] = None,
    raceTo: Option[Int] = None,
    matches: List[PeriodMatch] = Nil
)

/** One match within a completed period file. */
final case class PeriodMatch(
    playerA: Player,
    playerB: Player,
    scoreA: Int,
    scoreB: Int,
    raceTo: Int,
    handicapSuggested: Int,
    handicapApplied: Int
)

/** Bracket match lifecycle: pending → ready → started → completed. */
enum BracketMatchState {
  case Pending, Ready, Started, Completed
}

/** One match slot in a double-elimination bracket. */
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

/** Full bracket tree for a tournament. */
final case class Bracket(
    size: Int,
    matches: List[BracketMatch]
)

/** Replay-derived tournament state. */
final case class TournamentState(
    name: String,
    players: List[Player],
    bracket: Option[Bracket] = None,
    frozenRatings: Map[Player, PlayerRating] = Map.empty,
    raceToByScope: Map[String, Int] = Map.empty,
    playersLocked: Boolean = false,
    completed: Boolean = false
)

// --- Tournament event payloads (v1) ---

final case class TournamentCreatedPayload(
    name: String,
    players: List[Player]
)

final case class PlayersSetPayload(
    players: List[Player]
)

final case class PlayersLockedPayload()

final case class RaceToSetPayload(
    scope: String,
    raceTo: Int
)

final case class BracketSeededPayload(
    frozenRatings: List[PlayerRating],
    bracket: Bracket
)

final case class MatchReadyPayload(
    matchId: String,
    handicapSuggested: Int
)

final case class HandicapAppliedPayload(
    matchId: String,
    handicapApplied: Int
)

final case class MatchStartedPayload(
    matchId: String
)

final case class MatchResultPayload(
    matchId: String,
    scoreA: Int,
    scoreB: Int
)

final case class TournamentCompletedPayload(
    completed: LocalDate
)
