# Todo: Bye Bracket Display Fix

## Phase 1: Data Layer

- [x] **Task 1** — Add `isBye: Boolean = false` to `BracketMatch` in `liga/model/Types.scala`
  - [x] Compiles: `sbt --client compile`

- [x] **Task 2** — Fix `BracketGen` cascade + `BracketSpec` tests
  - [x] Narrow `isByeMatch` to `wb-1-*` with one player only
  - [x] Set `isBye = true` in `propagateByes` after bye advance
  - [x] 3-in-4: `wb-1-1` completed, `isBye = true`, P1 in one pending `wb-2-*`, no cascade
  - [x] 12-in-16: four R1 byes with `isBye = true`, each winner in one pending `wb-2-*`
  - [x] 8-in-8: no `isBye = true` after generation
  - [x] Tests pass: `sbt --client "liga/testOnly ph.samson.atbp.liga.bracket.BracketSpec"`

## Checkpoint: Data Layer

- [x] BracketSpec green
- [x] Cascade stopped — bye winners only in immediate `wb-2-*` slot
- [x] Human review before UI work

## Phase 2: Presentation Layer

- [x] **Task 3** — JS model mirror + `resultLabel` helper
  - [x] `isBye` on `liga-js/api/Models.BracketMatch`
  - [x] `BracketLayout.resultLabel(matchDef): Option[String]`
  - [x] Bye → `Some("bye")`, normal → `Some("7–4")`, pending → `None`
  - [x] Tests pass: `sbt --client "liga-js/testOnly ph.samson.atbp.liga.js.director.BracketLayoutSpec"`

- [x] **Task 4** — Views, match panel, CSS
  - [x] `BracketView` uses `resultLabel` with `match-bye` / `match-score` classes
  - [x] `AudienceBracketView` uses `resultLabel` with `match-bye` / `match-score` classes
  - [x] `MatchPanel` completed state: "Bye — auto-advance" when `isBye`
  - [x] `.match-bye` CSS in `director.css` and `AudienceApp.scala`
  - [x] Tests pass: `sbt --client "liga-js/test"`

## Checkpoint: Complete

- [x] `sbt --client "liga/test"` passes
- [x] `sbt --client "liga-js/test"` passes
- [x] `sbt --client fixup` with clean `git status`
- [ ] Manual: 12-in-16 shows `bye` on director + audience R1 rows
- [ ] Ready for review
