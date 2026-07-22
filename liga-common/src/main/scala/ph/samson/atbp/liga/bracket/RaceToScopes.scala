package ph.samson.atbp.liga.bracket

/** Scope keys for section-aware race-to configuration (`wb-3`, `lb-4`, `gf`).
  */
object RaceToScopes {

  enum Section(val label: String, val order: Int) {
    case Winners extends Section("Winners Bracket", 0)
    case Losers extends Section("Losers Bracket", 1)
    case GrandFinal extends Section("Grand Final", 2)
  }

  final case class ScopeLabel(section: Section, roundLabel: String)

  def keyForMatch(matchId: String): Option[String] =
    matchId match {
      case s"wb-$round-$_" => Some(s"wb-$round")
      case s"lb-$round-$_" => Some(s"lb-$round")
      case "gf-1"          => Some("gf")
      case _               => None
    }

  def requiredKeys(playerCount: Int): List[String] = {
    val size = TournamentBounds.bracketSize(playerCount)
    val wb = (1 to winnersRounds(size)).map(n => s"wb-$n")
    val lb = (1 to losersRounds(size)).map(n => s"lb-$n")
    wb.toList ++ lb.toList :+ "gf"
  }

  def scopeLabel(scope: String): ScopeLabel =
    scope match {
      case s"wb-$round" =>
        ScopeLabel(Section.Winners, s"Round $round")
      case s"lb-$round" =>
        ScopeLabel(Section.Losers, s"Round $round")
      case "gf" =>
        ScopeLabel(Section.GrandFinal, "Grand Final")
      case _ =>
        ScopeLabel(Section.Winners, scope)
    }

  private def winnersRounds(bracketSize: Int): Int =
    log2(bracketSize)

  private def losersRounds(bracketSize: Int): Int =
    (winnersRounds(bracketSize) - 1) * 2

  private def log2(n: Int): Int =
    (math.log(n) / math.log(2)).toInt
}
