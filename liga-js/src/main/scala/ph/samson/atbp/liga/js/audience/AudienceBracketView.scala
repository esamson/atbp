package ph.samson.atbp.liga.js.audience

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.js.director.BracketLayout

/** Read-only bracket display for the audience screen. */
object AudienceBracketView {

  def apply(bracket: Bracket): Div = {
    val groups = BracketLayout.groupMatches(bracket.matches, bracket.size)
    div(
      cls := "audience-bracket",
      groups.map { group =>
        div(
          cls := "bracket-section",
          h2(BracketLayout.groupLabel(group.section, group.round)),
          div(
            cls := "round-matches",
            group.matches.map(matchRow)
          )
        )
      }
    )
  }

  private def matchRow(matchDef: BracketMatch): Div = {
    val isLive = matchDef.state == BracketMatchState.Started
    val isReady = matchDef.state == BracketMatchState.Ready
    div(
      cls := {
        val base = List("match-row")
        val withLive = if (isLive) base :+ "live" else base
        if (isReady) (withLive :+ "ready").mkString(" ")
        else withLive.mkString(" ")
      },
      span(cls := "match-players", playersLabel(matchDef)),
      span(cls := "match-state", BracketLayout.stateLabel(matchDef.state)),
      matchDef.handicapApplied.filter(_ > 0).map { handicap =>
        span(cls := "match-handicap", s"spot $handicap")
      },
      BracketLayout.resultLabel(matchDef).map { label =>
        val labelClass = if (matchDef.isBye) "match-bye" else "match-score"
        span(cls := labelClass, label)
      }
    )
  }

  private def playersLabel(matchDef: BracketMatch): String = {
    val a = BracketLayout.playerLabel(matchDef.playerA)
    val b = BracketLayout.playerLabel(matchDef.playerB)
    s"$a vs $b"
  }
}
