package ph.samson.atbp.liga.bracket

/** Shared roster limits and bracket sizing for liga tournaments. */
object TournamentBounds {

  val MinPlayers: Int = 3
  val MaxPlayers: Int = 64

  def validPlayerCount(count: Int): Boolean =
    count >= MinPlayers && count <= MaxPlayers

  def invalidPlayerCountMessage(count: Int): String =
    s"player count must be $MinPlayers–$MaxPlayers: $count"

  /** Smallest power-of-two bracket that fits `playerCount` players. */
  def bracketSize(playerCount: Int): Int = {
    require(
      validPlayerCount(playerCount),
      invalidPlayerCountMessage(playerCount)
    )
    if (playerCount <= 4) {
      4
    } else if (playerCount <= 8) {
      8
    } else if (playerCount <= 16) {
      16
    } else if (playerCount <= 32) {
      32
    } else {
      64
    }
  }
}
