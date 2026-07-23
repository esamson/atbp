package ph.samson.atbp.liga.bracket

import ph.samson.atbp.liga.model.*

final case class AdvanceResult(
    bracket: Bracket,
    newlyReady: List[String]
)

/** Advance players through a double-elimination bracket. */
object Advancement {

  def advance(
      bracket: Bracket,
      matchId: String,
      winner: Player
  ): Either[String, AdvanceResult] =
    advance(bracket, matchId, winner, BracketTopology(bracket.size))

  def advance(
      bracket: Bracket,
      matchId: String,
      winner: Player,
      topology: BracketTopology.Topology
  ): Either[String, AdvanceResult] = {
    for {
      matchDef <- findMatch(bracket, matchId)
      _ <- validateWinner(matchDef, winner)
      loser <- loserOf(matchDef, winner)
      updated <- completeMatch(bracket, matchId, winner)
      placed <- placePlayers(updated, topology, matchId, winner, loser)
      propagated = BracketByes.propagateStructuralByes(placed, topology)
    } yield readyNewMatches(propagated, topology, matchId)
  }

  private def findMatch(
      bracket: Bracket,
      matchId: String
  ): Either[String, BracketMatch] =
    bracket.matches.find(_.id == matchId) match {
      case None        => Left(s"unknown match: $matchId")
      case Some(value) => Right(value)
    }

  private def validateWinner(
      matchDef: BracketMatch,
      winner: Player
  ): Either[String, Unit] =
    if (
      matchDef.playerA.contains(winner) || matchDef.playerB.contains(winner)
    ) {
      Right(())
    } else {
      Left(s"$winner is not a participant in ${matchDef.id}")
    }

  private def loserOf(
      matchDef: BracketMatch,
      winner: Player
  ): Either[String, Option[Player]] =
    Right {
      if (matchDef.playerA.contains(winner)) {
        matchDef.playerB
      } else {
        matchDef.playerA
      }
    }

  private def completeMatch(
      bracket: Bracket,
      matchId: String,
      winner: Player
  ): Either[String, Bracket] = {
    val updatedMatches = bracket.matches.map { matchDef =>
      if (matchDef.id == matchId) {
        val scoreA =
          if (matchDef.playerA.contains(winner)) 1 else 0
        val scoreB =
          if (matchDef.playerB.contains(winner)) 1 else 0
        matchDef.copy(
          state = BracketMatchState.Completed,
          result = Some(MatchResult(scoreA, scoreB))
        )
      } else {
        matchDef
      }
    }
    Right(bracket.copy(matches = updatedMatches))
  }

  private def placePlayers(
      bracket: Bracket,
      topology: BracketTopology.Topology,
      matchId: String,
      winner: Player,
      loser: Option[Player]
  ): Either[String, Bracket] = {
    val withWinner = place(bracket, topology.winnerTo.get(matchId), winner)
    val withLoser = loser.fold(withWinner) { player =>
      place(withWinner, topology.loserTo.get(matchId), player)
    }
    Right(withLoser)
  }

  private def place(
      bracket: Bracket,
      target: Option[(String, BracketTopology.Slot)],
      player: Player
  ): Bracket =
    target match {
      case None                   => bracket
      case Some((targetId, slot)) =>
        val updatedMatches = bracket.matches.map { matchDef =>
          if (matchDef.id == targetId) {
            val updated =
              slot match {
                case BracketTopology.Slot.A =>
                  matchDef.copy(playerA = Some(player))
                case BracketTopology.Slot.B =>
                  matchDef.copy(playerB = Some(player))
              }
            val state =
              if (updated.playerA.nonEmpty && updated.playerB.nonEmpty) {
                BracketMatchState.Ready
              } else {
                updated.state
              }
            updated.copy(state = state)
          } else {
            matchDef
          }
        }
        bracket.copy(matches = updatedMatches)
    }

  private def readyNewMatches(
      bracket: Bracket,
      topology: BracketTopology.Topology,
      completedMatchId: String
  ): AdvanceResult = {
    val affectedTargets =
      List(
        topology.winnerTo.get(completedMatchId),
        topology.loserTo.get(completedMatchId)
      ).flatten.map(_._1).distinct

    val newlyReady =
      affectedTargets.filter { targetId =>
        bracket.matches
          .find(_.id == targetId)
          .exists(_.state == BracketMatchState.Ready)
      }

    AdvanceResult(bracket, newlyReady)
  }
}
