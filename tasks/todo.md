# Todo: Bye Bracket Display Fix

## Phase 1: Data Layer

- [ ] **Task 1** — Add `isBye: Boolean = false` to `BracketMatch` in `liga/model/Types.scala`
  - [ ] Compiles: `sbt --client compile`

- [ ] **Task 2** — Fix `BracketGen` cascade + `BracketSpec` tests
  - [ ] Narrow `isByeMatch` to `wb-1-*` with one player only
  - [ ] Set `isBye = true` in `propagateByes` after bye advance
  - [ ] 3-in-4: `wb-1-1` completed, `isBye = true`, P1 in one pending `wb-2-*`, no cascade
  - [ ] 12-in-16: four R1 byes with `isBye = true`, each winner in one pending `wb-2-*`
  - [ ] 8-in-8: no `isBye = true` after generation
  - [ ] Tests pass: `sbt --client "liga/testOnly ph.samson.atbp.liga.bracket.BracketSpec"`

## Checkpoint: Data Layer

- [ ] BracketSpec green
- [ ] Cascade stopped — bye winners only in immediate `wb-2-*` slot
- [ ] Human review before UI work

## Phase 2: Presentation Layer

- [ ] **Task 3** — JS model mirror + `resultLabel` helper
  - [ ] `isBye` on `liga-js/api/Models.BracketMatch`
  - [ ] `BracketLayout.resultLabel(matchDef): Option[String]`
  - [ ] Bye → `Some("bye")`, normal → `Some("7–4")`, pending → `None`
  - [ ] Tests pass: `sbt --client "liga-js/testOnly ph.samson.atbp.liga.js.director.BracketLayoutSpec"`

- [ ] **Task 4** — Views, match panel, CSS
  - [ ] `BracketView` uses `resultLabel` with `match-bye` / `match-score` classes
  - [ ] `AudienceBracketView` uses `resultLabel` with `match-bye` / `match-score` classes
  - [ ] `MatchPanel` completed state: "Bye — auto-advance" when `isBye`
  - [ ] `.match-bye` CSS in `director.css` and `AudienceApp.scala`
  - [ ] Tests pass: `sbt --client "liga-js/test"`

## Checkpoint: Complete

- [ ] `sbt --client "liga/test"` passes
- [ ] `sbt --client "liga-js/test"` passes
- [ ] `sbt --client fixup` with clean `git status`
- [ ] Manual: 12-in-16 shows `bye` on director + audience R1 rows
- [ ] Ready for review
