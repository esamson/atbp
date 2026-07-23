package ph.samson.atbp.liga.bracket

import ph.samson.atbp.liga.model.*

/** Auto-advance structural byes wherever empty seed slots leave matches that
  * can never be contested (winners R1, ghost losers matches, and cascading
  * half-filled losers matches).
  */
object BracketByes {

  private enum FeederKind {
    case Winner, Loser
  }

  def propagateStructuralByes(
      bracket: Bracket,
      topology: BracketTopology.Topology
  ): Bracket = {
    def loop(current: Bracket): Bracket =
      findStructuralBye(current, topology) match {
        case None          => current
        case Some(matchId) =>
          val matchDef = current.matches.find(_.id == matchId).get
          matchDef.playerA.orElse(matchDef.playerB) match {
            case None =>
              // Ghost match: both feeders are dead; complete with no players.
              val marked = current.copy(
                matches = current.matches.map {
                  case m if m.id == matchId =>
                    m.copy(state = BracketMatchState.Completed, isBye = true)
                  case m => m
                }
              )
              loop(marked)
            case Some(winner) =>
              val after =
                Advancement
                  .advance(current, matchId, winner, topology)
                  .toOption
                  .get
              val marked = after.bracket.copy(
                matches = after.bracket.matches.map {
                  case m if m.id == matchId => m.copy(isBye = true)
                  case m                    => m
                }
              )
              loop(marked)
          }
      }
    loop(bracket)
  }

  private def findStructuralBye(
      bracket: Bracket,
      topology: BracketTopology.Topology
  ): Option[String] = {
    val byId = bracket.matches.map(m => m.id -> m).toMap
    bracket.matches
      .find(m =>
        isWinnersRound1Bye(m) ||
          isEmptyGhostBye(m, byId, topology) ||
          isHalfFilledUnfillable(m, byId, topology)
      )
      .map(_.id)
  }

  private def isWinnersRound1Bye(matchDef: BracketMatch): Boolean = {
    val hasA = matchDef.playerA.nonEmpty
    val hasB = matchDef.playerB.nonEmpty
    matchDef.id.startsWith("wb-1-") &&
    matchDef.state != BracketMatchState.Completed &&
    (hasA ^ hasB)
  }

  /** No players, and every inbound slot is permanently dead (e.g. lb-1 fed only
    * by WB R1 byes).
    */
  private def isEmptyGhostBye(
      matchDef: BracketMatch,
      byId: Map[String, BracketMatch],
      topology: BracketTopology.Topology
  ): Boolean =
    matchDef.state != BracketMatchState.Completed &&
      matchDef.playerA.isEmpty &&
      matchDef.playerB.isEmpty &&
      permanentlyUnfillable(matchDef.id, byId, topology, Set.empty)

  /** Sole player advances when the empty slot can never receive an opponent. */
  private def isHalfFilledUnfillable(
      matchDef: BracketMatch,
      byId: Map[String, BracketMatch],
      topology: BracketTopology.Topology
  ): Boolean = {
    if (matchDef.state == BracketMatchState.Completed) {
      false
    } else {
      val hasA = matchDef.playerA.nonEmpty
      val hasB = matchDef.playerB.nonEmpty
      if (!(hasA ^ hasB)) {
        false
      } else {
        val emptySlot =
          if (hasA) BracketTopology.Slot.B else BracketTopology.Slot.A
        slotUnfillable(matchDef.id, emptySlot, byId, topology, Set.empty)
      }
    }
  }

  /** True when this match can never acquire any players. */
  private def permanentlyUnfillable(
      matchId: String,
      byId: Map[String, BracketMatch],
      topology: BracketTopology.Topology,
      visiting: Set[String]
  ): Boolean = {
    if (visiting.contains(matchId)) {
      false
    } else {
      byId.get(matchId) match {
        case None    => false
        case Some(m) =>
          if (m.playerA.nonEmpty || m.playerB.nonEmpty) {
            false
          } else if (m.state == BracketMatchState.Completed) {
            m.isBye
          } else {
            val next = visiting + matchId
            List(BracketTopology.Slot.A, BracketTopology.Slot.B).forall {
              slot =>
                slotUnfillable(matchId, slot, byId, topology, next)
            }
          }
      }
    }
  }

  private def slotUnfillable(
      matchId: String,
      slot: BracketTopology.Slot,
      byId: Map[String, BracketMatch],
      topology: BracketTopology.Topology,
      visiting: Set[String]
  ): Boolean = {
    val feeders = feedersOf(topology, matchId, slot)
    feeders.nonEmpty && feeders.forall { case (src, kind) =>
      sourceCanNeverFill(src, kind, byId, topology, visiting)
    }
  }

  private def feedersOf(
      topology: BracketTopology.Topology,
      matchId: String,
      slot: BracketTopology.Slot
  ): List[(String, FeederKind)] = {
    val fromLosers = topology.loserTo.collect {
      case (src, (tgt, s)) if tgt == matchId && s == slot =>
        (src, FeederKind.Loser)
    }.toList
    val fromWinners = topology.winnerTo.collect {
      case (src, (tgt, s)) if tgt == matchId && s == slot =>
        (src, FeederKind.Winner)
    }.toList
    fromLosers ++ fromWinners
  }

  private def sourceCanNeverFill(
      sourceId: String,
      kind: FeederKind,
      byId: Map[String, BracketMatch],
      topology: BracketTopology.Topology,
      visiting: Set[String]
  ): Boolean =
    byId.get(sourceId) match {
      case None    => false
      case Some(m) =>
        kind match {
          case FeederKind.Loser =>
            // Bye matches never place a loser into the drop slot.
            m.isBye
          case FeederKind.Winner =>
            if (m.isBye && m.playerA.isEmpty && m.playerB.isEmpty) {
              true
            } else if (
              m.playerA.isEmpty && m.playerB.isEmpty &&
              m.state != BracketMatchState.Completed
            ) {
              permanentlyUnfillable(sourceId, byId, topology, visiting)
            } else {
              false
            }
        }
    }
}
