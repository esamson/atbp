package ph.samson.atbp.liga.js.audience

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.ApiClient
import ph.samson.atbp.liga.js.api.Models.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.Failure
import scala.util.Success

/** Read-only Laminar app for the audience display (poll-based refresh). */
object AudienceApp {

  private val DefaultPollSeconds = 5

  def apply(client: ApiClient): Div = {
    val tournament = Var[Option[TournamentResponse]](None)
    val pollSeconds = Var(DefaultPollSeconds)
    val lastUpdated = Var[Option[String]](None)
    val statusMessage = Var("")

    def refresh(): Unit = {
      client.getTournament.onComplete {
        case Success(value) =>
          tournament.set(Some(value))
          lastUpdated.set(Some(new js.Date().toLocaleTimeString()))
          statusMessage.set("")
        case Failure(err) =>
          statusMessage.set(err.getMessage)
      }
    }

    val pollTicks = pollSeconds.signal.flatMapSwitch { seconds =>
      EventStream.periodic(intervalMs = seconds * 1000)
    }

    div(
      cls := "audience-app",
      onMountCallback { _ =>
        client.getConfig.onComplete {
          case Success(config) =>
            pollSeconds.set(config.audiencePollIntervalSeconds)
            refresh()
          case Failure(_) =>
            refresh()
        }
      },
      pollTicks --> Observer[Int](_ => refresh()),
      header(
        tournament.signal,
        pollSeconds.signal,
        lastUpdated.signal
      ),
      child <-- statusMessage.signal.map { msg =>
        if (msg.nonEmpty) div(cls := "error", msg) else emptyNode
      },
      child <-- tournament.signal.map {
        case None =>
          div(cls := "loading", p("Loading tournament…"))
        case Some(t) =>
          TournamentPhase.fromApi(t.phase) match {
            case TournamentPhase.None =>
              div(
                cls := "empty",
                p("No active tournament yet.")
              )
            case TournamentPhase.Defining | TournamentPhase.Locked |
                TournamentPhase.RaceTo =>
              div(
                cls := "setup",
                h2(t.name),
                p("Tournament setup in progress…")
              )
            case TournamentPhase.Active | TournamentPhase.Completed =>
              t.bracket match {
                case Some(bracket) =>
                  div(
                    cls := "bracket-container",
                    if (t.completed) {
                      p(cls := "completed-banner", "Tournament complete")
                    } else {
                      emptyNode
                    },
                    AudienceBracketView(bracket)
                  )
                case None =>
                  div(p("Bracket not seeded yet."))
              }
          }
      },
      styleTag(audienceStyles)
    )
  }

  private def header(
      tournament: Signal[Option[TournamentResponse]],
      pollSeconds: Signal[Int],
      lastUpdated: Signal[Option[String]]
  ): Div =
    div(
      cls := "header",
      h1("Liga"),
      child <-- tournament.map { maybeTournament =>
        span(
          cls := "tournament-name",
          maybeTournament
            .map(_.name)
            .filter(_.nonEmpty)
            .getOrElse("Live bracket")
        )
      },
      child <-- pollSeconds.combineWith(lastUpdated).map {
        case (seconds, maybeTime) =>
          val timeLabel = maybeTime.map(t => s" · updated $t").getOrElse("")
          span(cls := "poll-status", s"Refreshing every ${seconds}s$timeLabel")
      }
    )

  private val audienceStyles: String =
    """
      |.audience-app {
      |  font-family: system-ui, sans-serif;
      |  margin: 1.5rem 2rem;
      |  max-width: 1400px;
      |  color: #111;
      |}
      |.header { margin-bottom: 1.5rem; }
      |.header h1 { margin: 0 0 0.25rem; font-size: 2rem; }
      |.tournament-name { font-size: 1.25rem; color: #444; }
      |.poll-status { display: block; font-size: 0.9rem; color: #666; margin-top: 0.25rem; }
      |.error { color: #b00020; margin-bottom: 1rem; }
      |.loading, .empty, .setup { font-size: 1.25rem; color: #555; }
      |.completed-banner {
      |  font-size: 1.1rem; font-weight: 600; color: #2e7d32;
      |  margin-bottom: 1rem; padding: 0.5rem 0.75rem;
      |  background: #e8f5e9; border-radius: 4px;
      |}
      |.bracket-section { margin-bottom: 1.5rem; }
      |.bracket-section h2 { font-size: 1.1rem; margin: 0 0 0.75rem; color: #333; }
      |.round-matches { display: flex; flex-direction: column; gap: 0.5rem; }
      |.match-row {
      |  display: grid; grid-template-columns: 1fr auto auto auto;
      |  gap: 1rem; align-items: center;
      |  padding: 0.6rem 0.9rem; border: 1px solid #ddd; border-radius: 6px;
      |  background: #fafafa; font-size: 1.1rem;
      |}
      |.match-row.live { border-left: 4px solid #c62828; background: #fff5f5; }
      |.match-row.ready { border-left: 4px solid #f9a825; }
      |.match-state { text-transform: uppercase; font-size: 0.8rem; color: #666; }
      |.match-handicap { font-size: 0.9rem; color: #555; }
      |.match-score { font-weight: 600; font-size: 1.2rem; }
      |.match-bye { font-style: italic; opacity: 0.75; }
      |""".stripMargin
}
