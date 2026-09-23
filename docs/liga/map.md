# Liga project map

Load only the section you need for the current task.

## Modules

| Module | Path | Owns |
|--------|------|------|
| `liga` | `liga/` | CLI renderers, Glicko2 engine, tournament lifecycle, HTTP serve, persistence |
| `liga-common` | `liga-common/` | Cross-compiled types, handicap math, race-to scopes/wizard, bounds, roster paste/order, Glicko tuning |
| `liga-js` | `liga-js/` | Laminar director + audience SPAs, API client models |

`liga` depends on the JVM `liga-common` matrix row and embeds `liga-js` fastLinkJS output under web resources.

## Domain (JVM + shared)

### Ratings / periods
- `liga/.../glicko/` — Glicko2, leaderboard
- `liga/.../io/` — period load/write, data layout, codecs
- `liga-common/.../glicko/Tuning.scala` — shared defaults (e.g. guest RD)
- Batch period rules: `docs/ideas/liga-batch-period-ratings.md`

### Handicap
- `liga-common/.../handicap/` — suggestion + win probability + cap (shared with JS preview)
- CLI: `liga/.../cli/HandicapRenderer.scala`
- Director hints: `docs/intent/director-handicap-probability-hints.md`

### Roster
- `liga-common/.../roster/` — paste parsing, rating order
- Soft-remove before lock: `docs/intent/liga-roll-call-roster-remove.md`
- Paste on define: `docs/intent/paste-roster.md`

### Bracket topology
- `liga/.../bracket/` — gen, seeding, advancement, byes
- `liga-common/.../bracket/` — `RaceToScopes`, `RaceToWizard`, `TournamentBounds`
- Section race-to: `docs/ideas/section-aware-race-to.md`
- Race-to display: `docs/intent/race-to-display-and-hints.md`

### Tournament lifecycle
- `liga/.../tournament/` — phases, events, replay/resume, validation, period emission
- Match flow: ready → apply handicap → start → record result (or forfeit)
- Forfeit: no period scores; opponent advances as loss path — `docs/intent/liga-match-forfeit.md`
- `liga/.../tournament/MatchWaitTiming.scala` — API-only wait/Done Instant
  projection from the event log (not on domain `BracketMatch`)
- Completed bracket results (W–L + rating Δ on last round): `docs/intent/liga-bracket-results.md`
- Elimination cut (last 1 true DE / last \(2^k\) DE→SE): `docs/intent/liga-elimination-cut.md`

## Serve / HTTP

- `liga/.../serve/` — `Server`, `Routes`, `DirectorRoutes`, `ServeContext`, static assets
- Director write APIs + audience/director read APIs
- Persistence: append-only JSON under tournament data dir
- Tournament JSON bracket matches may include optional `waitStartedAt` /
  `completedAt` (ISO Instants) from `MatchWaitTiming`; domain model unchanged
- Idle serve (no active tournament → Latest Ratings): `docs/intent/serve-without-tournament.md`

## Director UI (`liga-js`)

Active-phase layout (`DirectorApp.mainLayout`): CSS grid — bracket column + match panel column (`director.css` `.main-layout`).

| File | Role |
|------|------|
| `director/DirectorApp.scala` | Root app, phase routing, selection, actions |
| `director/IdleDirectorView.scala` | Idle / Latest Ratings when no tournament |
| `director/BracketView.scala` | Match list by section/round; ~15s elapsed chip tick |
| `director/BracketLayout.scala` | Grouping/order, director Ready strip, elapsed chip helpers |
| `director/AppliedHandicapView.scala` | Names + handicap markers + rating subline |
| `director/MatchScoreView.scala` | Shared score/bye rendering for director + audience brackets |
| `director/MatchPanel.scala` | Ready / handicap / start / score / forfeit controls |
| `director/WizardView.scala` | Pre-seed setup (roster, race-to, seed) |
| `director/RosterSoftRemove.scala` | Roll-call soft-remove before Save/Lock |
| `director/RaceToLabels.scala` | `"Race to N"` from resolved scope |
| `director/HandicapProbabilityHints.scala` | CLI-parity win-% neighborhood in MatchPanel |
| `director/ForfeitSubmitPolicy.scala` | Forfeit submit gating |
| `director/BracketResults.scala` | Post-complete W–L / rating Δ labels |
| `director/BracketResultsContext.scala` | Results context from completed tournament |
| `director/DirectorGuidance.scala` | Copy / error hints |
| `api/Client.scala` + `Models.scala` | HTTP client + DTOs (incl. optional wait fields) |

Audience bracket order (`BracketLayout.groupMatches`): unfinished rounds above
fully Completed rounds; within each band Grand Final → Losers → Winners, later
rounds above earlier; within a round Ready → Pending → Started → Completed then
`match.id`. Empty Pending slots with neither player are hidden. Intent:
`docs/intent/director-audience-bracket-order.md`.

Director list (`BracketLayout.directorGroupMatches`): Live strip at top
(`Started`, round then seed index) with static started-time chip from
`startedAt`; Ready strip below sorted longest-wait first (equal wait → earlier
rounds, then seed index); lower list = `groupMatches` on non-Live ∩ non-Ready.
Elapsed `HH:mm` chip from `waitStartedAt` (Ready + Pending-with-one) or
`completedAt` (Done; none on bye). Intent:
`docs/intent/td-match-wait-indicators.md`.

Player rating subline (director + audience via `AppliedHandicapView`):
`docs/intent/player-ratings-in-bracket.md`.

## Audience UI (`liga-js`)

Separate entry (`MainAudience`); poll-based; read-only. Same SPA shell and
`audience.js` bundle for both routes; `AudienceApp` branches on pathname.

| Route | View | Role |
|-------|------|------|
| `/audience` | `AudienceBracketView` | **Default** list bracket (unchanged) |
| `/audience/bracket` | `AudienceSpatialBracketView` | Opt-in chalkboard columns + section bands |

Serve: `Routes.scala` serves `StaticAssets.audienceHtml` at both paths.
Path constants: `AudienceRoute.SpatialBracketPath`; director footer reuses via
`DirectorGuidance.audienceListPath` / `audienceBracketPath`.

| File | Role |
|------|------|
| `audience/AudienceApp.scala` | Poll shell, pathname branch list vs spatial, `spatial-*` chalkboard CSS in `styleTag` |
| `audience/AudienceRoute.scala` | Pathname normalize + `isSpatialBracket` |
| `audience/AudienceBracketView.scala` | List view — `BracketLayout.groupMatches` order; ratings via `AppliedHandicapView` |
| `audience/AudienceSpatialBracketView.scala` | Spatial bands/columns/cells; Club density (`showRatings = false`); subtle live/done classes |
| `audience/AudienceSpatialLayout.scala` | Pure column/band geometry — unfinished left, completed right; WB/GF/SE above LB; shared LCM slot grid aligns later rounds between feeders |
| `audience/AudienceIdlePolicy.scala` | Idle / Latest Ratings (both routes) |

Spatial layout reuses `BracketLayout.showInList` visibility (hide Pending with
neither player); **ordering** is spatial-only (not `groupMatches`). Columns in a
band share an LCM slot grid so later-round cells center on feeder pairs; hidden
bye slots in the payload keep seed rows even when the cell is not shown. Intent:
`docs/intent/spatial-bracket.md`, `docs/ideas/spatial-bracket.md`.

## Docs index

| Doc | Use when |
|-----|----------|
| `docs/intent/liga.md` | Confirmed product intent |
| `docs/ideas/liga.md` | Architecture direction + MVP in/out |
| `docs/ideas/section-aware-race-to.md` | Per-scope race-to |
| `docs/ideas/liga-batch-period-ratings.md` | Period batching |
| `docs/ideas/bye-bracket-display.md` | Bye display |
| `docs/ideas/grand-final-labels.md` | GF labeling |
| `docs/intent/paste-roster.md` | Roster paste |
| `docs/intent/liga-roll-call-roster-remove.md` | Soft-remove before lock |
| `docs/intent/serve-without-tournament.md` | Serve without active tournament |
| `docs/intent/director-audience-bracket-order.md` | Director + audience list visibility/order |
| `docs/intent/td-match-wait-indicators.md` | TD wait / Done elapsed + Ready strip |
| `docs/ideas/td-match-wait-indicators.md` | Wait-indicator direction / MVP in-out |
| `docs/intent/race-to-display-and-hints.md` | Race-to labels + validation numbers |
| `docs/intent/director-handicap-probability-hints.md` | MatchPanel win-% hints |
| `docs/intent/player-ratings-in-bracket.md` | Rating subline / unrated guests |
| `docs/intent/liga-match-forfeit.md` | Forfeit (no period scores) |
| `docs/intent/liga-bracket-results.md` | Post-complete bracket results |
| `docs/intent/liga-elimination-cut.md` | DE→SE cut / true DE (last N) |
| `docs/intent/spatial-bracket.md` | Spatial audience bracket intent |
| `docs/ideas/spatial-bracket.md` | Spatial bracket direction |
| `docs/ideas/liga-match-forfeit.md` | Forfeit direction |
| `docs/ideas/liga-bracket-results.md` | Bracket results direction |
| `docs/ideas/liga-roll-call-roster-remove.md` | Roll-call remove direction |

## Known UX pressure (Active phase)

Large brackets bury live work under early/irrelevant slots; director scroll can take the side match panel out of view. Shipped list visibility/order: `docs/intent/director-audience-bracket-order.md` (panel placement unchanged).
