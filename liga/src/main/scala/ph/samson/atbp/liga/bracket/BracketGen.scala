package ph.samson.atbp.liga.bracket

import ph.samson.atbp.liga.model.*

/** Generate a double-elimination bracket from seeded player ratings. */
object BracketGen {

  def generate(players: List[PlayerRating]): Bracket = {
    val size = Seeding.bracketSize(players.size)
    val seeds = Seeding.seedOrder(players)
    val seedSlots = seedSlotsFor(size, seeds)
    val topology = BracketTopology(size)
    val matches =
      topology.matches.toList.sortBy(_._1).map { case (id, defn) =>
        val (playerA, playerB) = resolveFeeders(defn, seedSlots, Map.empty)
        emptyMatch(id, playerA, playerB)
      }
    val bracket = Bracket(size, matches)
    propagateByes(bracket, topology)
  }

  private def seedSlotsFor(
      size: Int,
      seeds: List[PlayerRating]
  ): Map[Int, Option[Player]] = {
    val slotSeeds = Seeding.bracketSlotSeeds(size)
    slotSeeds.map { seedNumber =>
      val player =
        if (seedNumber <= seeds.size) {
          Some(seeds(seedNumber - 1).player)
        } else {
          None
        }
      seedNumber -> player
    }.toMap
  }

  private def resolveFeeders(
      defn: BracketTopology.MatchDef,
      seedSlots: Map[Int, Option[Player]],
      winners: Map[String, Player]
  ): (Option[Player], Option[Player]) = {
    (
      resolveFeeder(defn.feederA, seedSlots, winners),
      resolveFeeder(defn.feederB, seedSlots, winners)
    )
  }

  private def resolveFeeder(
      feeder: BracketTopology.Feeder,
      seedSlots: Map[Int, Option[Player]],
      winners: Map[String, Player]
  ): Option[Player] =
    feeder match {
      case BracketTopology.Feeder.Empty        => None
      case BracketTopology.Feeder.Seed(number) =>
        seedSlots.getOrElse(number, None)
      case BracketTopology.Feeder.WinnerOf(id) => winners.get(id)
      case BracketTopology.Feeder.LoserOf(_)   => None
    }

  private def emptyMatch(
      id: String,
      playerA: Option[Player],
      playerB: Option[Player]
  ): BracketMatch = {
    val state =
      if (playerA.nonEmpty && playerB.nonEmpty) {
        BracketMatchState.Ready
      } else {
        BracketMatchState.Pending
      }
    BracketMatch(id, playerA, playerB, state)
  }

  private def propagateByes(
      bracket: Bracket,
      topology: BracketTopology.Topology
  ): Bracket = {
    def loop(current: Bracket): Bracket = {
      val byeMatch = current.matches.find(isByeMatch).map(_.id)
      byeMatch match {
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
    }
    loop(bracket)
  }

  private def isByeMatch(bracketMatch: BracketMatch): Boolean = {
    val hasA = bracketMatch.playerA.nonEmpty
    val hasB = bracketMatch.playerB.nonEmpty
    bracketMatch.id.startsWith("wb-1-") &&
    bracketMatch.state != BracketMatchState.Completed &&
    (hasA ^ hasB)
  }
}
