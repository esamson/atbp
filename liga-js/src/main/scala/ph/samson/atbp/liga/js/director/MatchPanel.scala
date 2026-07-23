package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.handicap.Handicap
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.model as shared

/** Match control panel: ready, handicap, start, and result entry. */
object MatchPanel {

  def apply(
      tournament: TournamentResponse,
      matchDef: BracketMatch,
      busy: Signal[Boolean],
      onReady: Observer[Unit],
      onApplyHandicap: Observer[Int],
      onStart: Observer[Unit],
      onResult: Observer[(Int, Int)]
  ): Div = {
    val handicapInput = Var(
      matchDef.handicapApplied
        .orElse(matchDef.handicapSuggested)
        .map(_.toString)
        .getOrElse(previewHandicap(tournament, matchDef).handicap.toString)
    )
    val scoreAInput = Var(
      matchDef.result.map(_.scoreA.toString).getOrElse("0")
    )
    val scoreBInput = Var(
      matchDef.result.map(_.scoreB.toString).getOrElse("0")
    )
    val validationError = Var("")

    div(
      cls := "match-panel",
      h2(
        BracketLayout.matchLabel(
          matchDef.id,
          tournament.bracket.map(_.size).getOrElse(8)
        )
      ),
      p(cls := "match-id", matchDef.id),
      p(
        s"${BracketLayout.playerLabel(matchDef.playerA)} vs ${BracketLayout.playerLabel(matchDef.playerB)}"
      ),
      p(
        cls := "match-state",
        s"State: ${BracketLayout.stateLabel(matchDef.state)}"
      ),
      matchDef.raceTo.map(rt => p(s"Race to $rt")),
      p(cls := "guidance", DirectorGuidance.matchStepHint(matchDef)),
      matchControls(
        matchDef,
        tournament,
        handicapInput,
        scoreAInput,
        scoreBInput,
        validationError,
        busy,
        onReady,
        onApplyHandicap,
        onStart,
        onResult
      )
    )
  }

  private def matchControls(
      matchDef: BracketMatch,
      tournament: TournamentResponse,
      handicapInput: Var[String],
      scoreAInput: Var[String],
      scoreBInput: Var[String],
      validationError: Var[String],
      busy: Signal[Boolean],
      onReady: Observer[Unit],
      onApplyHandicap: Observer[Int],
      onStart: Observer[Unit],
      onResult: Observer[(Int, Int)]
  ): Div =
    matchDef.state match {
      case BracketMatchState.Pending =>
        div(p("Waiting for players."))

      case BracketMatchState.Ready if matchDef.handicapSuggested.isEmpty =>
        val preview = previewHandicap(tournament, matchDef)
        div(
          p(
            DirectorGuidance.handicapSpotLabel(
              preview.weakerPlayer.name,
              preview.raceTo,
              DirectorGuidance.handicapCap(preview.raceTo)
            )
          ),
          p(s"Preview spot: ${preview.handicap}"),
          button(
            cls := "primary",
            disabled <-- busy,
            onClick.mapTo(()) --> onReady,
            "Ready match"
          )
        )

      case BracketMatchState.Ready =>
        val raceTo = matchDef.raceTo.getOrElse(7)
        val cap = DirectorGuidance.handicapCap(raceTo)
        val weakerName = weakerPlayerName(tournament, matchDef)
        div(
          weakerName
            .map(name =>
              p(DirectorGuidance.handicapSpotLabel(name, raceTo, cap))
            )
            .getOrElse(emptyNode),
          label(
            "Handicap (games spotted to weaker player): ",
            input(
              typ := "number",
              value <-- handicapInput,
              onInput.mapToValue --> handicapInput
            )
          ),
          matchDef.handicapSuggested.map { suggested =>
            p(s"Server suggested: $suggested")
          },
          div(
            cls := "actions",
            button(
              disabled <-- busy,
              onClick --> onApplyHandicap.contramap { _ =>
                handicapInput.now().toIntOption.getOrElse(0)
              },
              "Apply handicap"
            ),
            button(
              cls := "primary",
              disabled <-- busy.combineWith(handicapInput.signal).map {
                case (isBusy, _) =>
                  isBusy || matchDef.handicapApplied.isEmpty
              },
              onClick.mapTo(()) --> onStart,
              "Start match"
            )
          ),
          Option.when(matchDef.handicapApplied.isEmpty)(
            p(cls := "hint", "Apply handicap before starting.")
          )
        )

      case BracketMatchState.Started =>
        div(
          matchDef.handicapApplied
            .flatMap { h =>
              weakerPlayerName(tournament, matchDef).map { name =>
                p(s"Handicap applied: $h spot to $name")
              }
            }
            .getOrElse(emptyNode),
          p(cls := "guidance", DirectorGuidance.scoreboardScoreHint),
          div(
            cls := "score-entry",
            label(
              s"${BracketLayout.playerLabel(matchDef.playerA)}: ",
              input(
                typ := "number",
                value <-- scoreAInput,
                onInput.mapToValue --> scoreAInput
              )
            ),
            label(
              s"${BracketLayout.playerLabel(matchDef.playerB)}: ",
              input(
                typ := "number",
                value <-- scoreBInput,
                onInput.mapToValue --> scoreBInput
              )
            )
          ),
          child <-- validationError.signal.map { msg =>
            if (msg.nonEmpty) div(cls := "validation-error", msg) else emptyNode
          },
          button(
            cls := "primary",
            disabled <-- busy,
            onClick.mapTo(()) --> Observer[Unit] { _ =>
              val scoreA = scoreAInput.now().toIntOption
              val scoreB = scoreBInput.now().toIntOption
              (scoreA, scoreB) match {
                case (Some(a), Some(b)) if a == b =>
                  validationError.set(
                    "Scores cannot tie — one player must win."
                  )
                case (Some(a), Some(b)) if a < 0 || b < 0 =>
                  validationError.set("Scores must be zero or greater.")
                case (Some(a), Some(b)) =>
                  validationError.set("")
                  onResult.onNext((a, b))
                case _ =>
                  validationError.set("Enter valid scores for both players.")
              }
            },
            "Record result"
          )
        )

      case BracketMatchState.Completed =>
        if (matchDef.isBye) {
          div(
            p("Bye — auto-advance"),
            matchDef.handicapApplied.map(h => p(s"Handicap was $h"))
          )
        } else {
          matchDef.result match {
            case Some(result) =>
              div(
                p(s"Final score: ${result.scoreA}–${result.scoreB}"),
                matchDef.handicapApplied.map(h => p(s"Handicap was $h"))
              )
            case None =>
              div(p("Match completed."))
          }
        }
    }

  private def weakerPlayerName(
      tournament: TournamentResponse,
      matchDef: BracketMatch
  ): Option[String] =
    previewHandicap(tournament, matchDef).weakerPlayer.name match {
      case name if name.nonEmpty => Some(name)
      case _                     => None
    }

  private def previewHandicap(
      tournament: TournamentResponse,
      matchDef: BracketMatch
  ): HandicapSuggestion = {
    val raceTo = matchDef.raceTo.orElse(
      BracketLayout.defaultRaceTo(matchDef.id, tournament.raceToByScope)
    )
    val ratingA = matchDef.playerA.flatMap(p =>
      tournament.frozenRatings.find(_.player.name == p.name)
    )
    val ratingB = matchDef.playerB.flatMap(p =>
      tournament.frozenRatings.find(_.player.name == p.name)
    )
    (for {
      a <- ratingA
      b <- ratingB
      rt <- raceTo
    } yield toJsSuggestion(
      Handicap.suggest(toSharedRating(a), toSharedRating(b), rt)
    ))
      .getOrElse(HandicapSuggestion(Player("—"), handicap = 0, raceTo = 7))
  }

  private def toSharedRating(rating: PlayerRating): shared.PlayerRating =
    shared.PlayerRating(
      shared.Player(rating.player.name),
      rating.rating,
      rating.rd,
      rating.wins,
      rating.losses
    )

  private def toJsSuggestion(
      suggestion: shared.HandicapSuggestion
  ): HandicapSuggestion =
    HandicapSuggestion(
      Player(suggestion.weakerPlayer.name),
      suggestion.handicap,
      suggestion.raceTo
    )
}
