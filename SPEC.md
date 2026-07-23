# Spec: Bye Bracket Display Fix

Source idea: [docs/ideas/bye-bracket-display.md](docs/ideas/bye-bracket-display.md)

## Objective

**Problem:** When a tournament has fewer players than the bracket size (e.g. 12 in a 16), top seeds receive round-1 byes. Two bugs follow:

1. **Cascade:** `BracketGen.isByeMatch` treats any half-filled incomplete match as a bye. After an R1 bye, the winner lands in `wb-2-*` alone — still half-filled — so `propagateByes` recursively auto-completes through every later winners round.
2. **Misleading display:** Completed R1 bye matches render as `1–0`, indistinguishable from a real one-rack win. Directors and audience cannot tell a bye from a played match.

**Goal:** (a) Stop bye cascade so winners appear only in their immediate next match; (b) label completed R1 bye matches clearly as **bye** on director and audience surfaces.

**Target users:** Tournament directors and audience members viewing bracket state in Liga.

**User stories:**

- As a director, when I seed a 12-player tournament into a 16 bracket, P1–P4 show as completed byes in R1 and appear only in their `wb-2-*` slots — not pre-filled through later rounds.
- As a director, I see `bye` (not `1–0`) on completed R1 bye rows and in the match detail panel.
- As an audience member, I see the same: bye winners in the next round only, with R1 bye matches labeled `bye`.

**Acceptance criteria:**

1. R1 winners bye matches (`wb-1-*` with one empty seed slot) are auto-completed during generation; internal scores remain 1–0 for advancement logic.
2. After bye propagation, each bye winner appears in exactly one `wb-2-*` match in `Pending` state.
3. Later half-filled matches (waiting for a feeder winner) are **not** treated as byes.
4. Completed R1 bye matches carry `isBye = true` in bracket data.
5. Director bracket rows, audience bracket rows, and director match panel show **bye** instead of `1–0` for bye matches.
6. Normal completed matches still show `scoreA–scoreB`.
7. Tests cover cascade fix (3-in-4, 12-in-16) and bye labeling (unit + layout).

## Assumptions

1. R1 winners byes are the only structural empty slots in partial-fill brackets.
2. `wb-2-*` with one player should stay `Pending` until the feeder completes.
3. Losers-bracket bye rules do not need changes unless a case surfaces.
4. Label text is lowercase `bye`, consistent with existing state labels (`pending`, `ready`, `live`, `done`).
5. `isBye` defaults to `false` so existing event replays remain compatible.

→ Correct any of these before implementation if they are wrong.

## Tech Stack

- **Language:** Scala 3
- **Build:** sbt (multi-module monorepo)
- **Test framework:** ZIO Test (`zio.test.*`)
- **UI:** Laminar (`liga-js`)
- **Affected modules:** `liga` (bracket generation + model), `liga-js` (director + audience rendering)

## Commands

```bash
# Compile
sbt --client compile

# Bracket generation tests
sbt --client "liga/testOnly ph.samson.atbp.liga.bracket.BracketSpec"

# UI layout tests
sbt --client "liga-js/testOnly ph.samson.atbp.liga.js.director.BracketLayoutSpec"

# Full test suites
sbt --client "liga/test"
sbt --client "liga-js/test"

# Format + lint gate (required before commit)
sbt --client fixup
```

## Project Structure

```
liga/src/main/scala/ph/samson/atbp/liga/
  model/Types.scala           → add isBye: Boolean = false to BracketMatch
  bracket/BracketGen.scala    → narrow isByeMatch; set isBye on bye completion

liga/src/test/scala/ph/samson/atbp/liga/bracket/
  BracketSpec.scala           → cascade + isBye assertions

liga-js/src/main/scala/ph/samson/atbp/liga/js/
  api/Models.scala            → mirror isBye on BracketMatch
  director/BracketLayout.scala → resultLabel(matchDef): Option[String]
  director/BracketView.scala  → use resultLabel
  director/MatchPanel.scala   → bye copy in completed state
  audience/AudienceBracketView.scala → use resultLabel
  audience/AudienceApp.scala  → optional .match-bye styling

liga-js/src/test/scala/ph/samson/atbp/liga/js/director/
  BracketLayoutSpec.scala     → resultLabel tests
```

## Code Style

**BracketGen — narrow bye detection:**

```scala
private def isByeMatch(bracketMatch: BracketMatch): Boolean = {
  val hasA = bracketMatch.playerA.nonEmpty
  val hasB = bracketMatch.playerB.nonEmpty
  bracketMatch.id.startsWith("wb-1-") &&
    bracketMatch.state != BracketMatchState.Completed &&
    (hasA ^ hasB)
}
```

**BracketGen — mark bye after advance** (in `propagateByes`):

```scala
val after = Advancement.advance(current, matchId, winner, topology).toOption.get
val marked = after.bracket.copy(
  matches = after.bracket.matches.map {
    case m if m.id == matchId => m.copy(isBye = true)
    case m                    => m
  }
)
loop(marked)
```

**BracketLayout — shared display helper:**

```scala
def resultLabel(matchDef: BracketMatch): Option[String] =
  matchDef.result.map { result =>
    if (matchDef.isBye) "bye"
    else s"${result.scoreA}–${result.scoreB}"
  }
```

**BracketView / AudienceBracketView** — replace inline score formatting:

```scala
BracketLayout.resultLabel(matchDef).map { label =>
  val cls = if (matchDef.isBye) "match-bye" else "match-score"
  span(cls := cls, label)
}
```

**MatchPanel** — completed state (copy: **"Bye — auto-advance"**):

```scala
case Some(_) if matchDef.isBye =>
  div(p("Bye — auto-advance"))
case Some(result) =>
  div(p(s"Final score: ${result.scoreA}–${result.scoreB}"))
```

**CSS** — `.match-bye` on director and audience surfaces: muted color + italic (distinct from `.match-score`):

```css
.match-bye { font-style: italic; opacity: 0.75; }
```

Conventions:

- Keep 1–0 scores in domain model for advancement; only *display* changes.
- One helper (`resultLabel`) — do not duplicate bye/score logic across views.
- Match existing Laminar patterns and `BracketLayout` pure-helper style.

## Testing Strategy

| Level | Location | Coverage |
|-------|----------|----------|
| Unit | `BracketSpec` | Cascade fix; `isBye = true` on completed R1 byes; `isBye = false` on normal matches |
| Unit | `BracketLayoutSpec` | `resultLabel` returns `"bye"` vs `"7–4"` |
| Integration | `liga/test` | No regressions in tournament lifecycle |
| Manual | Director + audience UI | 12-in-16 bracket shows `bye` on R1 top-seed rows |

**BracketSpec additions:**

1. **3-in-4** — `wb-1-1` completed with `isBye = true`; P1 in one pending `wb-2-*`; no cascade.
2. **12-in-16** — four R1 byes with `isBye = true`; each winner in one pending `wb-2-*`.
3. **8-in-8** (full bracket) — no matches have `isBye = true` after generation.

**BracketLayoutSpec additions:**

- Bye match → `resultLabel` == `Some("bye")`
- Normal completed match → `resultLabel` == `Some("7–4")`
- Pending match → `resultLabel` == `None`

## Boundaries

**Always:**

- Run `sbt --client fixup` with clean `git status` before committing (per `AGENTS.md`).
- Fix cascade in `BracketGen` (data layer); label byes in UI (presentation layer).
- Use `isBye` flag as source of truth for display, not score heuristics alone.
- Keep R1 bye matches visible in the bracket (completed, labeled `bye`).
- Extend existing test files.

**Ask first:**

- Losers-bracket bye labeling or detection.
- Changing advancement/score semantics (bye matches should still store 1–0 internally).
- API versioning or migration for persisted tournament events.

**Never:**

- UI-only cascade fix (filtering/deduplication without `BracketGen` change).
- "Full cascade then scrub" approach.
- Hiding R1 bye matches from the director view.
- Showing `1–0` on bye matches when `isBye` is true.

## Success Criteria

- [ ] `isByeMatch` only matches first-round winners (`wb-1-*`) with one player.
- [ ] Completed R1 bye matches have `isBye = true`; normal matches have `isBye = false`.
- [ ] Director bracket, audience bracket, and match panel show `bye` for bye matches.
- [ ] `sbt --client "liga/test"` and `sbt --client "liga-js/test"` pass.
- [ ] `sbt --client fixup` exits successfully with clean working tree.

## Out of Scope

- Display-layer deduplication of cascaded players (addressed by data fix, not filtering).
- Losers-bracket bye rules (unless a case surfaces during testing).
- Hiding or collapsing R1 bye rows.
- Grand-final or other label changes.

## Resolved Decisions

| Question | Decision |
|----------|----------|
| Match panel copy | `"Bye — auto-advance"` |
| Bye label styling | `.match-bye` — muted (`opacity: 0.75`) + italic on director and audience bracket rows |

If assumptions prove wrong during implementation, pause and update this spec before expanding scope.
