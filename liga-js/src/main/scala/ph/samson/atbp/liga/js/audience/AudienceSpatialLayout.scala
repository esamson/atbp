package ph.samson.atbp.liga.js.audience

import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.api.Models.BracketMatchState
import ph.samson.atbp.liga.js.director.BracketLayout
import ph.samson.atbp.liga.js.director.BracketLayout.Section

/** Pure spatial column/band layout for the audience chalkboard bracket. */
object AudienceSpatialLayout {

  final case class SpatialPlacedMatch(
      matchDef: BracketMatch,
      gridRowStart: Int,
      gridRowSpan: Int
  ) {
    def gridRowCss: String = s"$gridRowStart / span $gridRowSpan"
  }

  final case class SpatialColumn(
      round: Int,
      placed: List[SpatialPlacedMatch],
      roundSize: Int
  ) {
    def matches: List[BracketMatch] = placed.map(_.matchDef)
  }

  final case class SpatialBand(
      section: Section,
      columns: List[SpatialColumn],
      slotCount: Int
  )

  /** CSS grid row (1-based) and span for a match in the slot grid (column
    * header sits outside this grid in the view). `slotRank` is 1-based among
    * all matches in the round, including hidden ones.
    */
  def matchGridRow(
      slotRank: Int,
      roundSize: Int,
      slotCount: Int
  ): (Int, Int) = {
    val span = slotCount / math.max(roundSize, 1)
    val start = 1 + (slotRank - 1) * span
    (start, span)
  }

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a.abs else gcd(b, a % b)

  private def lcm(a: Int, b: Int): Int =
    if (a == 0 || b == 0) 0
    else (a.abs / gcd(a, b)) * b.abs

  private def bandSlotCount(roundSizes: List[Int]): Int =
    roundSizes.filter(_ > 0).reduceOption(lcm).getOrElse(1)

  private final case class PreparedColumn(
      section: Section,
      round: Int,
      allMatches: List[BracketMatch],
      shownMatches: List[BracketMatch]
  )

  /** Slot rank and `roundSize` come from `allMatches` in the API payload, not
    * from theoretical `bracketSize >> round`. Omitted match ids (as opposed to
    * hidden ghost byes still present in the payload) would mis-rank visible
    * cells off their feeder rows.
    */
  private def placedColumn(
      prepared: PreparedColumn,
      slotCount: Int
  ): SpatialColumn = {
    val roundSize = prepared.allMatches.size
    val ranked =
      prepared.allMatches.sortBy(m => BracketLayout.matchSeedIndex(m.id))
    SpatialColumn(
      round = prepared.round,
      placed = prepared.shownMatches.map { matchDef =>
        val rank = ranked.indexWhere(_.id == matchDef.id) + 1
        val (start, span) = matchGridRow(rank, roundSize, slotCount)
        SpatialPlacedMatch(matchDef, start, span)
      },
      roundSize = roundSize
    )
  }

  private def roundFullyCompleted(matches: List[BracketMatch]): Boolean =
    matches.forall(_.state == BracketMatchState.Completed)

  private def spatialBandOrder(section: Section): Int =
    section match {
      case Section.GrandFinal        => 0
      case Section.SingleElimination => 1
      case Section.Winners           => 2
      case Section.Losers            => 3
    }

  /** Column sort: unfinished left, completed right; higher rounds left within
    * each completion bucket.
    */
  private def columnTimelineKey(
      round: Int,
      allMatches: List[BracketMatch]
  ): (Int, Int) =
    (
      if (roundFullyCompleted(allMatches)) 1 else 0,
      -round
    )

  /** Bands stacked top→bottom (Winners/GF/SE above Losers); columns left→right
    * on a shared current-left timeline.
    */
  def layout(
      matches: List[BracketMatch],
      bracketSize: Int
  ): List[SpatialBand] =
    matches
      .groupBy(m =>
        (
          BracketLayout.sectionOf(m.id),
          BracketLayout.roundOf(m.id, bracketSize)
        )
      )
      .toList
      .map { case ((section, round), grouped) =>
        val roundVisible = grouped.exists(BracketLayout.showForAudience)
        PreparedColumn(
          section = section,
          round = round,
          allMatches = grouped,
          shownMatches = grouped
            .filter(m => BracketLayout.showInVisibleRound(m, roundVisible))
            .sortBy(m => BracketLayout.matchSeedIndex(m.id))
        )
      }
      .filter(_.shownMatches.nonEmpty)
      .groupBy(_.section)
      .toList
      .sortBy { case (section, _) => spatialBandOrder(section) }
      .map { case (section, prepared) =>
        val slotCount = bandSlotCount(prepared.map(_.allMatches.size))
        SpatialBand(
          section = section,
          columns = prepared
            .sortBy(p => columnTimelineKey(p.round, p.allMatches))
            .map(placedColumn(_, slotCount)),
          slotCount = slotCount
        )
      }
}
