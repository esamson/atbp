package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.bracket.RaceToScopes
import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.api.Models.BracketMatchState

/** Pure bracket layout helpers for the director UI. */
object BracketLayout {

  enum Section(val label: String, val order: Int) {
    case Winners extends Section("Winners", 0)
    case Losers extends Section("Losers", 1)
    case GrandFinal extends Section("Grand Final", 2)
  }

  final case class RoundGroup(
      section: Section,
      round: Int,
      matches: List[BracketMatch]
  )

  def sectionOf(matchId: String): Section =
    matchId match {
      case s if s.startsWith("wb-") => Section.Winners
      case s if s.startsWith("lb-") => Section.Losers
      case _                        => Section.GrandFinal
    }

  def groupLabel(section: Section, round: Int): String =
    section match {
      case Section.GrandFinal => section.label
      case _                  => s"${section.label} — round $round"
    }

  def matchLabel(matchId: String, bracketSize: Int): String =
    groupLabel(sectionOf(matchId), roundOf(matchId, bracketSize))

  private def log2(n: Int): Int =
    (math.log(n) / math.log(2)).toInt

  /** Bracket round encoded in match ids (`wb-2-1`, `lb-3-2`, `gf-1`). */
  def bracketRound(matchId: String, bracketSize: Int): Option[Int] =
    matchId match {
      case s"wb-$round-$_" => round.toIntOption
      case s"lb-$round-$_" => round.toIntOption
      case "gf-1"          => Some(log2(bracketSize))
      case _               => None
    }

  def roundOf(matchId: String, bracketSize: Int): Int =
    bracketRound(matchId, bracketSize).getOrElse(0)

  def groupMatches(
      matches: List[BracketMatch],
      bracketSize: Int
  ): List[RoundGroup] =
    matches
      .groupBy(m => (sectionOf(m.id), roundOf(m.id, bracketSize)))
      .toList
      .sortBy { case ((section, round), _) => (section.order, round) }
      .map { case ((section, round), grouped) =>
        RoundGroup(section, round, grouped.sortBy(_.id))
      }

  def isActionable(matchDef: BracketMatch): Boolean =
    matchDef.state == BracketMatchState.Ready ||
      matchDef.state == BracketMatchState.Started

  def allMatchesCompleted(matches: List[BracketMatch]): Boolean =
    matches.nonEmpty && matches.forall(_.state == BracketMatchState.Completed)

  def playerLabel(
      player: Option[ph.samson.atbp.liga.js.api.Models.Player]
  ): String =
    player.map(_.name).getOrElse("—")

  def stateLabel(state: BracketMatchState): String =
    state match {
      case BracketMatchState.Pending   => "pending"
      case BracketMatchState.Ready     => "ready"
      case BracketMatchState.Started   => "live"
      case BracketMatchState.Completed => "done"
    }

  def defaultRaceTo(
      matchId: String,
      raceToByScope: Map[String, Int]
  ): Option[Int] =
    RaceToScopes.keyForMatch(matchId).flatMap(raceToByScope.get)

  def resultLabel(matchDef: BracketMatch): Option[String] =
    matchDef.result.map { result =>
      if (matchDef.isBye) "bye"
      else s"${result.scoreA}–${result.scoreB}"
    }
}
