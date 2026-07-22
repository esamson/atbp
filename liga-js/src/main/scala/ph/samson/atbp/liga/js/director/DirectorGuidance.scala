package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.bracket.TournamentBounds
import ph.samson.atbp.liga.handicap.HandicapCap
import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.api.Models.BracketMatchState

/** Director-facing copy for match setup and API error translation. */
object DirectorGuidance {

  val matchWorkflowOverview: String =
    "Each match: Ready (compute spot) → Apply handicap → Start → Record result."

  val scoreboardScoreHint: String =
    "Enter the numbers on the scoreboard (include spotted games). " +
      "Example: Bob spotted 2, board reads 7–5 → enter 7 and 5."

  val localhostNote: String =
    "Director controls are localhost-only. Open /audience on the club TV."

  def lockRosterHint(playerCount: Int): String =
    if (playerCount < TournamentBounds.MinPlayers) {
      s"Need at least ${TournamentBounds.MinPlayers} players to lock (currently $playerCount)."
    } else if (playerCount > TournamentBounds.MaxPlayers) {
      s"At most ${TournamentBounds.MaxPlayers} players allowed (currently $playerCount)."
    } else {
      ""
    }

  val lockSavesHint: String =
    "Lock saves your roster."

  val applyPasteHint: String =
    "Apply paste to update the preview — or Lock will use your pasted list."

  def seedHint: String =
    "Seeding freezes each player's rating from the period leaderboard. " +
      "Guests receive default ratings."

  def matchStepHint(matchDef: BracketMatch): String =
    matchDef.state match {
      case BracketMatchState.Pending =>
        "Waiting for both players to be assigned from earlier results."
      case BracketMatchState.Ready if matchDef.handicapSuggested.isEmpty =>
        "Step 1 of 4: click Ready to compute a handicap spot from frozen ratings."
      case BracketMatchState.Ready if matchDef.handicapApplied.isEmpty =>
        "Step 2 of 4: adjust the spot if needed, then Apply handicap."
      case BracketMatchState.Ready =>
        "Step 3 of 4: Start match once the spot is agreed."
      case BracketMatchState.Started =>
        "Step 4 of 4: enter scoreboard totals and record the result."
      case BracketMatchState.Completed =>
        "Match finished."
    }

  def handicapCap(raceTo: Int): Int =
    HandicapCap.capFor(raceTo)

  def handicapSpotLabel(weakerName: String, raceTo: Int, cap: Int): String =
    s"Spot for $weakerName in race-to-$raceTo (max $cap)."

  def friendlyApiError(raw: String): String = {
    val lower = raw.toLowerCase
    if (lower.contains("handicap must be applied first")) {
      "Apply a handicap before starting play."
    } else if (lower.contains("handicap already suggested")) {
      "Handicap is already computed — adjust and apply, or start after applying."
    } else if (lower.contains("match must be readied first")) {
      "Click Ready match first to compute a handicap suggestion."
    } else if (lower.contains("both players must be assigned")) {
      "Both players must be in the bracket before you can ready this match."
    } else if (lower.contains("scores cannot tie")) {
      "Scores cannot tie — one player must win."
    } else if (lower.contains("scores must be non-negative")) {
      "Scores must be zero or greater."
    } else if (lower.contains("duplicate player names")) {
      "Each player name must be unique on the roster."
    } else if (lower.contains("handicap must be at most")) {
      "Handicap exceeds the maximum allowed for this race-to."
    } else if (lower.contains("handicap must be non-negative")) {
      "Handicap cannot be negative."
    } else if (lower.contains("winner score must be")) {
      "Winner's scoreboard total must match the race-to for this round."
    } else if (lower.contains("loser score must be less than")) {
      "Loser's scoreboard total must be below the race-to."
    } else if (lower.startsWith("player count must be")) {
      s"Roster must have ${TournamentBounds.MinPlayers}–${TournamentBounds.MaxPlayers} players"
    } else {
      raw
    }
  }
}
