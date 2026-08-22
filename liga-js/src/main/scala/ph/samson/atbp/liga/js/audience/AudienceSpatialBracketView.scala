package ph.samson.atbp.liga.js.audience

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.js.director.AppliedHandicapLabels
import ph.samson.atbp.liga.js.director.AppliedHandicapView
import ph.samson.atbp.liga.js.director.BracketHandicapContext
import ph.samson.atbp.liga.js.director.BracketLayout
import ph.samson.atbp.liga.js.director.BracketResultsContext
import ph.samson.atbp.liga.js.director.MatchScoreView

/** Spatial column bracket for the audience display. */
object AudienceSpatialBracketView {

  def cellStateClass(state: BracketMatchState): String =
    state match {
      case BracketMatchState.Started   => "spatial-cell-live"
      case BracketMatchState.Completed => "spatial-cell-done"
      case BracketMatchState.Ready     => "spatial-cell-ready"
      case BracketMatchState.Pending   => ""
    }

  def cellClassList(state: BracketMatchState): List[String] = {
    val stateClass = cellStateClass(state)
    if (stateClass.isEmpty) List("spatial-cell")
    else List("spatial-cell", stateClass)
  }

  def apply(
      bracket: Bracket,
      seRounds: Int,
      handicapContext: BracketHandicapContext,
      resultsContext: BracketResultsContext
  ): Div = {
    val bands = AudienceSpatialLayout.layout(bracket.matches, bracket.size)
    div(
      cls := "spatial-bracket",
      bands.map { band =>
        div(
          cls := "spatial-band",
          h2(band.section.label),
          div(
            cls := "spatial-columns",
            styleAttr := s"--spatial-slots: ${band.slotCount}",
            band.columns.map { column =>
              div(
                cls := "spatial-column",
                h3(
                  BracketLayout.groupLabel(band.section, column.round, seRounds)
                ),
                div(
                  cls := "spatial-column-slots",
                  column.placed.map { placed =>
                    spatialCell(
                      handicapContext,
                      resultsContext,
                      placed.matchDef
                    )
                      .amend(styleAttr := s"grid-row: ${placed.gridRowCss}")
                  }
                )
              )
            }
          )
        )
      }
    )
  }

  /** Completed matches show score/bye between players instead of "vs". */
  def spatialUsesInlineResult(matchDef: BracketMatch): Boolean =
    matchDef.state == BracketMatchState.Completed

  private def spatialInlineMiddle(matchDef: BracketMatch): Option[Node] =
    if (spatialUsesInlineResult(matchDef)) {
      MatchScoreView(matchDef)
    } else {
      None
    }

  private def spatialCell(
      handicapContext: BracketHandicapContext,
      resultsContext: BracketResultsContext,
      matchDef: BracketMatch
  ): Div =
    div(
      cls := cellClassList(matchDef.state).mkString(" "),
      span(
        cls := "spatial-players",
        AppliedHandicapView.playersWithAppliedHandicap(
          matchDef,
          handicapContext,
          resultsContext,
          AppliedHandicapLabels.forMatch(handicapContext, matchDef),
          BracketLayout.winnerSide(matchDef),
          showRatings = false,
          inlineMiddle = spatialInlineMiddle(matchDef)
        )
      )
    )
}
