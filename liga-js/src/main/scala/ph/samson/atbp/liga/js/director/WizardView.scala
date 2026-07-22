package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.bracket.RaceToScopes
import ph.samson.atbp.liga.bracket.RaceToWizard
import ph.samson.atbp.liga.bracket.TournamentBounds
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.model.Player as CommonPlayer
import ph.samson.atbp.liga.model.PlayerRating as CommonPlayerRating
import ph.samson.atbp.liga.roster.RosterEntry
import ph.samson.atbp.liga.roster.RosterPaste

/** Define → Lock → Race-to → Seed wizard steps. */
object WizardView {

  def apply(
      tournament: TournamentResponse,
      leaderboard: LeaderboardResponse,
      busy: Signal[Boolean],
      onSetPlayers: Observer[List[Player]],
      onSaveAndLock: Observer[List[Player]],
      onSetRaceTo: Observer[Map[String, Int]],
      onSeed: Observer[Unit]
  ): Div = {
    val phase = TournamentPhase.fromApi(tournament.phase)
    phase match {
      case TournamentPhase.Defining =>
        definingStep(tournament, leaderboard, busy, onSetPlayers, onSaveAndLock)
      case TournamentPhase.Locked =>
        raceToStep(tournament, busy, onSetRaceTo)
      case TournamentPhase.RaceTo =>
        seedStep(tournament, busy, onSeed)
      case other =>
        div(p(s"Unexpected wizard phase: $other"))
    }
  }

  private def definingStep(
      tournament: TournamentResponse,
      leaderboard: LeaderboardResponse,
      busy: Signal[Boolean],
      onSetPlayers: Observer[List[Player]],
      onSaveAndLock: Observer[List[Player]]
  ): Div = {
    val names = Var(tournament.players.map(_.name))
    val pasteText = Var(tournament.players.map(_.name).mkString("\n"))

    val pasteDirty: Signal[Boolean] =
      pasteText.signal
        .combineWith(names.signal)
        .map { case (text, applied) =>
          RosterPaste.parsePaste(text) != applied
        }

    val lockCount: Signal[Int] =
      pasteText.signal.map(text => RosterPaste.parsePaste(text).size)

    val periodByName: Map[String, CommonPlayerRating] =
      leaderboard.ratings
        .map(r =>
          r.player.name -> CommonPlayerRating(
            player = CommonPlayer(r.player.name),
            rating = r.rating,
            rd = r.rd,
            wins = r.wins,
            losses = r.losses
          )
        )
        .toMap

    val rosterSignal: Signal[List[RosterEntry]] =
      names.signal.map(RosterPaste.resolveRoster(_, periodByName))

    div(
      cls := "wizard-panel",
      h2("Define roster"),
      p(tournament.name).amend(cls := "tournament-title"),
      p(
        "Paste the signup list (one name per line). " +
          "Exact matches keep period ratings; others show as guests. " +
          s"Lock when you have ${TournamentBounds.MinPlayers}–${TournamentBounds.MaxPlayers} players."
      ),
      div(
        cls := "roster-paste",
        label(
          "Signup names",
          textArea(
            rows := 8,
            placeholder := "Alice\nBob\nCarol",
            controlled(
              value <-- pasteText.signal,
              onInput.mapToValue --> pasteText.writer
            )
          )
        ),
        button(
          disabled <-- pasteText.signal.map(_.trim.isEmpty),
          onClick.mapTo(()) --> Observer[Unit] { _ =>
            names.set(RosterPaste.parsePaste(pasteText.now()))
          },
          "Apply paste"
        ),
        child <-- pasteText.signal.combineWith(pasteDirty).map {
          case (text, dirty) =>
            val count = RosterPaste.parsePaste(text).size
            val hints = List.newBuilder[Node]
            if (dirty) {
              hints += p(cls := "hint", DirectorGuidance.applyPasteHint)
            }
            if (count > 0) {
              val lockHint = DirectorGuidance.lockRosterHint(count)
              if (lockHint.nonEmpty) {
                hints += p(cls := "hint", lockHint)
              }
            }
            if (hints.result().isEmpty) emptyNode
            else div(hints.result())
        }
      ),
      div(
        cls := "roster-list",
        h3("Roster"),
        children <-- rosterSignal.map { entries =>
          if (entries.isEmpty) {
            List(p(cls := "hint", "No players yet — paste a signup list."))
          } else {
            entries.map { entry =>
              div(
                cls := (if (entry.guest) "roster-row guest" else "roster-row"),
                span(cls := "roster-name", entry.name),
                span(cls := "roster-rating", f"${entry.rating}%.0f"),
                if (entry.guest) span(cls := "guest-badge", "guest")
                else emptyNode
              )
            }
          }
        }
      ),
      div(
        cls := "roster-summary",
        child.text <-- names.signal.map(ns => s"${ns.size} players in roster"),
        child <-- pasteDirty.map { dirty =>
          if (!dirty) {
            p(cls := "hint", DirectorGuidance.lockSavesHint)
          } else {
            emptyNode
          }
        },
        child <-- names.signal.combineWith(pasteDirty).map { case (ns, dirty) =>
          if (!dirty) {
            val hint = DirectorGuidance.lockRosterHint(ns.size)
            if (hint.nonEmpty) p(cls := "hint", hint) else emptyNode
          } else {
            emptyNode
          }
        }
      ),
      div(
        cls := "wizard-actions",
        button(
          cls := "primary",
          disabled <-- busy,
          onClick.mapTo(()) --> Observer[Unit] { _ =>
            onSetPlayers.onNext(names.now().map(Player(_)))
          },
          "Save roster"
        ),
        button(
          disabled <-- busy.combineWith(lockCount).map { case (isBusy, count) =>
            isBusy || !TournamentBounds.validPlayerCount(count)
          },
          onClick.mapTo(()) --> Observer[Unit] { _ =>
            val rosterNames = RosterPaste.parsePaste(pasteText.now())
            names.set(rosterNames)
            onSaveAndLock.onNext(rosterNames.map(Player(_)))
          },
          "Lock roster"
        )
      )
    )
  }

  private def raceToStep(
      tournament: TournamentResponse,
      busy: Signal[Boolean],
      onSetRaceTo: Observer[Map[String, Int]]
  ): Div = {
    val playerCount = tournament.players.size
    val scopes = RaceToScopes.requiredKeys(playerCount)
    val initialWizard =
      if (tournament.raceToByScope.nonEmpty) {
        RaceToWizard.loadState(tournament.raceToByScope, playerCount)
      } else {
        RaceToWizard.initialState(playerCount)
      }
    val wizardState = Var(initialWizard)

    def scopesFor(section: RaceToScopes.Section): List[String] =
      scopes.filter(scope => RaceToScopes.scopeLabel(scope).section == section)

    def renderSection(section: RaceToScopes.Section): Div =
      div(
        cls := "race-to-section",
        h3(section.label),
        ul(
          cls := "race-to-inputs",
          scopesFor(section).map { scope =>
            val scopeLabel = RaceToScopes.scopeLabel(scope)
            li(
              label(
                scopeLabel.roundLabel,
                input(
                  typ := "number",
                  controlled(
                    value <-- wizardState.signal.map(
                      _.raceToByScope.getOrElse(scope, 7).toString
                    ),
                    onInput.mapToValue --> Observer[String] { raw =>
                      raw.toIntOption.foreach { value =>
                        wizardState.update { state =>
                          RaceToWizard.applyEdit(
                            state,
                            scope,
                            value,
                            playerCount
                          )
                        }
                      }
                    }
                  )
                ),
                if (scope == "gf") {
                  p(
                    cls := "hint",
                    "usually longer than finals — set explicitly."
                  )
                } else {
                  emptyNode
                }
              )
            )
          }
        )
      )

    div(
      cls := "wizard-panel",
      h2("Race-to by bracket section"),
      p(
        "Set race-to for winners, losers, and Grand Final. " +
          "Editing a round cascades through later rounds in that section."
      ),
      renderSection(RaceToScopes.Section.Winners),
      renderSection(RaceToScopes.Section.Losers),
      renderSection(RaceToScopes.Section.GrandFinal),
      button(
        cls := "primary",
        disabled <-- busy,
        onClick.mapTo(()) --> Observer[Unit](_ =>
          onSetRaceTo.onNext(wizardState.now().raceToByScope)
        ),
        "Save race-to"
      )
    )
  }

  private def seedStep(
      tournament: TournamentResponse,
      busy: Signal[Boolean],
      onSeed: Observer[Unit]
  ): Div =
    div(
      cls := "wizard-panel",
      h2("Seed bracket"),
      p(
        s"${tournament.players.size} players locked. Race-to configured for all bracket sections."
      ),
      p(cls := "guidance", DirectorGuidance.seedHint),
      ul(
        tournament.players.map(player => li(player.name))
      ),
      button(
        cls := "primary",
        disabled <-- busy,
        onClick.mapTo(()) --> onSeed,
        "Seed bracket"
      )
    )
}
