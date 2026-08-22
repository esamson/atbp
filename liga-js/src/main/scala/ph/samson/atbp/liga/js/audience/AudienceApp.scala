package ph.samson.atbp.liga.js.audience

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import ph.samson.atbp.liga.bracket.BracketFormat
import ph.samson.atbp.liga.js.LatestRatingsView
import ph.samson.atbp.liga.js.api.ApiClient
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.js.director.BracketHandicapContext
import ph.samson.atbp.liga.js.director.BracketResultsContext

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.util.Failure
import scala.util.Success

/** Read-only Laminar app for the audience display (poll-based refresh). */
object AudienceApp {

  private val DefaultPollSeconds = 5

  def apply(client: ApiClient): Div = {
    val tournament = Var[Option[TournamentResponse]](None)
    val latestRatings = Var[Option[LatestRatingsResponse]](None)
    val pollSeconds = Var(DefaultPollSeconds)
    val lastUpdated = Var[Option[String]](None)
    val statusMessage = Var("")

    def refresh(): Unit = {
      val loaded = client.getTournament.flatMap { t =>
        val phase = TournamentPhase.fromApi(t.phase)
        if (
          AudienceIdlePolicy.shouldFetchLatestRatingsOnRefresh(
            phase,
            latestRatings.now()
          )
        ) {
          client.getLatestRatings
            .map(lr => AudienceIdlePolicy.idleLatestRatingsFeed(Right(lr)))
            .recover { case err =>
              AudienceIdlePolicy.idleLatestRatingsFeed(Left(err.getMessage))
            }
            .map { case (feed, msg) => (t, Some(feed), msg) }
        } else if (AudienceIdlePolicy.needsLatestRatings(phase)) {
          // Completed with a cached post-period feed: keep it across polls.
          Future.successful((t, latestRatings.now(), Option.empty[String]))
        } else {
          Future.successful((t, None, Option.empty[String]))
        }
      }
      loaded.onComplete {
        case Success((t, lr, errMsg)) =>
          tournament.set(Some(t))
          latestRatings.set(lr)
          lastUpdated.set(Some(new js.Date().toLocaleTimeString()))
          statusMessage.set(errMsg.getOrElse(""))
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
      child <-- tournament.signal
        .combineWith(latestRatings.signal)
        .map { case (maybeTournament, maybeLatestRatings) =>
          AudienceIdlePolicy.view(maybeTournament, maybeLatestRatings) match {
            case AudienceIdlePolicy.View.LoadingTournament =>
              div(cls := "loading", p("Loading tournament…"))
            case AudienceIdlePolicy.View.LoadingLatestRatings =>
              div(cls := "loading", p("Loading latest ratings…"))
            case AudienceIdlePolicy.View.Idle(latest) =>
              LatestRatingsView(latest)
            case AudienceIdlePolicy.View.Setup(t) =>
              div(
                cls := "setup",
                h2(t.name),
                p("Tournament setup in progress…")
              )
            case AudienceIdlePolicy.View.Bracket(t) =>
              val resultsContext = maybeLatestRatings
                .map(BracketResultsContext.fromTournament(t, _))
                .getOrElse(BracketResultsContext.inactive(t))
              t.bracket match {
                case Some(bracket) =>
                  val seRounds =
                    BracketFormat.forBracket(bracket.size, t.topN).seRounds
                  val handicapContext = BracketHandicapContext.fromTournament(t)
                  val bracketView =
                    if (
                      AudienceRoute
                        .isSpatialBracket(dom.window.location.pathname)
                    ) {
                      AudienceSpatialBracketView(
                        bracket,
                        seRounds,
                        handicapContext,
                        resultsContext
                      )
                    } else {
                      AudienceBracketView(
                        bracket,
                        seRounds,
                        handicapContext,
                        resultsContext
                      )
                    }
                  div(
                    cls := "bracket-container",
                    if (t.completed) {
                      p(cls := "completed-banner", "Tournament complete")
                    } else {
                      emptyNode
                    },
                    bracketView
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
      |  display: grid; grid-template-columns: 1fr auto auto;
      |  gap: 1rem; align-items: center;
      |  padding: 0.6rem 0.9rem; border: 1px solid #ddd; border-radius: 6px;
      |  background: #fafafa; font-size: 1.1rem;
      |}
      |.match-row.live { border-left: 4px solid #c62828; background: #fff5f5; }
      |.match-row.ready { border-left: 4px solid #f9a825; }
      |.match-state { text-transform: uppercase; font-size: 0.8rem; color: #666; }
      |.match-vs-handicap { font-size: 0.85em; font-weight: 700; color: #c62828; }
      |.match-score { font-weight: 600; font-size: 1.2rem; }
      |.match-winner { font-weight: 700; color: #2e7d32; font-size: 1.3em; }
      |.player-cell {
      |  display: inline-flex;
      |  flex-direction: column;
      |  align-items: center;
      |  vertical-align: middle;
      |}
      |.player-rating {
      |  font-size: 0.7em;
      |  color: #888;
      |  font-variant-numeric: tabular-nums;
      |  line-height: 1.1;
      |}
      |.player-rating.rating-up { color: #2e7d32; }
      |.player-rating.rating-down { color: #c62828; }
      |.player-rating.rating-new { color: #8d6e00; font-weight: 600; }
      |.results-last { font-weight: 700; font-size: 1.2em; }
      |.results-prior { font-weight: 400; color: #888; opacity: 0.75; }
      |.match-winner.results-prior {
      |  color: #2e7d32;
      |  font-weight: 400;
      |  font-size: 1em;
      |}
      |.match-bye { font-style: italic; opacity: 0.75; }
      |.match-forfeit { font-style: italic; opacity: 0.75; }
      |
      |/* Spatial bracket — namespaced; list selectors above unchanged */
      |.audience-app:has(.spatial-bracket) {
      |  max-width: none;
      |}
      |.spatial-bracket {
      |  margin-top: 0.5rem;
      |}
      |.spatial-band + .spatial-band {
      |  margin-top: 1.75rem;
      |  padding-top: 1.5rem;
      |  border-top: 1px solid #ddd;
      |}
      |.spatial-band h2 {
      |  margin: 0 0 1rem;
      |  font-size: 1.1rem;
      |  font-weight: 600;
      |  color: #333;
      |}
      |.spatial-columns {
      |  display: flex;
      |  flex-direction: row;
      |  align-items: stretch;
      |  gap: 2.5rem;
      |}
      |.spatial-column {
      |  flex: 0 0 auto;
      |  min-width: 11rem;
      |  display: flex;
      |  flex-direction: column;
      |}
      |.spatial-column h3 {
      |  margin: 0 0 0.75rem;
      |  font-size: 0.95rem;
      |  font-weight: 600;
      |  color: #333;
      |  white-space: nowrap;
      |}
      |.spatial-column-slots {
      |  display: grid;
      |  grid-template-rows:
      |    repeat(var(--spatial-slots, 1), minmax(2.5rem, 1fr));
      |  row-gap: 0.75rem;
      |  flex: 1;
      |}
      |.spatial-cell {
      |  position: relative;
      |  display: flex;
      |  flex-direction: row;
      |  align-items: baseline;
      |  align-self: center;
      |  padding: 0.3rem 0.6rem;
      |  border: 1px solid #ddd;
      |  border-radius: 6px;
      |  background: #fafafa;
      |  font-size: 1rem;
      |  line-height: 1.2;
      |  white-space: nowrap;
      |}
      |.spatial-column:not(:last-child) .spatial-cell::after {
      |  content: "";
      |  position: absolute;
      |  top: 50%;
      |  right: -1.35rem;
      |  width: 1.1rem;
      |  height: 2px;
      |  background: #ddd;
      |  transform: translateY(-50%);
      |}
      |.spatial-players {
      |  font-weight: 500;
      |}
      |.spatial-cell .match-score {
      |  font-weight: 600;
      |  font-size: 1.05rem;
      |  font-variant-numeric: tabular-nums;
      |}
      |.spatial-cell-live {
      |  border-left: 4px solid #c62828;
      |  background: #fff5f5;
      |}
      |.spatial-cell-ready {
      |  border-left: 4px solid #f9a825;
      |}
      |.spatial-cell-done .spatial-players {
      |  color: #888;
      |  opacity: 0.75;
      |}
      |""".stripMargin
}
