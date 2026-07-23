package ph.samson.atbp.liga.bracket

/** Shared roster limits and bracket sizing for liga tournaments. */
object TournamentBounds {

  val MinPlayers: Int = 3
  val MaxPlayers: Int = 64

  def validPlayerCount(count: Int): Boolean =
    count >= MinPlayers && count <= MaxPlayers

  def invalidPlayerCountMessage(count: Int): String =
    s"player count must be $MinPlayers–$MaxPlayers: $count"

  private val BracketSizes: List[Int] = {
    val sizes = Iterator.iterate(4)(_ * 2).takeWhile(_ <= MaxPlayers).toList
    require(
      sizes.lastOption.contains(MaxPlayers),
      s"bracket sizes must reach MaxPlayers ($MaxPlayers)"
    )
    sizes
  }

  /** Smallest power-of-two bracket that fits `playerCount` players. */
  def bracketSize(playerCount: Int): Int = {
    require(
      validPlayerCount(playerCount),
      invalidPlayerCountMessage(playerCount)
    )
    BracketSizes.find(_ >= playerCount).get
  }
}
