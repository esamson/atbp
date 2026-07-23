package ph.samson.atbp.liga.bracket

import ph.samson.atbp.liga.model.*

/** Auto-advance structural byes in winners and losers round 1. */
object BracketByes {

  def propagateStructuralByes(
      bracket: Bracket,
      topology: BracketTopology.Topology
  ): Bracket = {
    def loop(current: Bracket): Bracket =
      findStructuralBye(current, topology) match {
        case None          => current
        case Some(matchId) =>
          val matchDef = current.matches.find(_.id == matchId).get
          val winner = matchDef.playerA.orElse(matchDef.playerB).get
          val after =
            Advancement.advance(current, matchId, winner, topology).toOption.get
          val marked = after.bracket.copy(
            matches = after.bracket.matches.map {
              case m if m.id == matchId => m.copy(isBye = true)
              case m                    => m
            }
          )
          loop(marked)
      }
    loop(bracket)
  }

  private def findStructuralBye(
      bracket: Bracket,
      topology: BracketTopology.Topology
  ): Option[String] =
    bracket.matches
      .find(m =>
        isWinnersRound1Bye(m) ||
          isLosersRound1StructuralBye(m, bracket, topology)
      )
      .map(_.id)

  private def isWinnersRound1Bye(matchDef: BracketMatch): Boolean = {
    val hasA = matchDef.playerA.nonEmpty
    val hasB = matchDef.playerB.nonEmpty
    matchDef.id.startsWith("wb-1-") &&
    matchDef.state != BracketMatchState.Completed &&
    (hasA ^ hasB)
  }

  /** LB R1 slot whose WB R1 feeder was a bye will never fill; sole player
    * advances.
    */
  private def isLosersRound1StructuralBye(
      matchDef: BracketMatch,
      bracket: Bracket,
      topology: BracketTopology.Topology
  ): Boolean = {
    if (!matchDef.id.startsWith("lb-1-")) {
      false
    } else if (matchDef.state == BracketMatchState.Completed) {
      false
    } else {
      val hasA = matchDef.playerA.nonEmpty
      val hasB = matchDef.playerB.nonEmpty
      if (!(hasA ^ hasB)) {
        false
      } else {
        val byId = bracket.matches.map(m => m.id -> m).toMap
        topology.loserTo.exists { case (wbId, (lbId, slot)) =>
          lbId == matchDef.id && wbId.startsWith("wb-1-") && {
            val wbBye = byId.get(wbId).exists(_.isBye)
            val slotEmpty = slot match {
              case BracketTopology.Slot.A => matchDef.playerA.isEmpty
              case BracketTopology.Slot.B => matchDef.playerB.isEmpty
            }
            wbBye && slotEmpty
          }
        }
      }
    }
  }
}
