# Implementation Plan: Bye Bracket Display Fix

## Overview

Fix two related bugs when a tournament has fewer players than bracket size (e.g. 12 in a 16): (1) `BracketGen.propagateByes` cascades through all later winners rounds because `isByeMatch` treats any half-filled match as a bye; (2) completed R1 bye matches display as `1–0`, indistinguishable from real wins. The fix narrows bye detection to `wb-1-*` only, marks completed bye matches with `isBye = true`, and updates director and audience surfaces to show **bye** instead of a score.

## Architecture Decisions

- **Data-first cascade fix** — Stop the bug in `BracketGen`, not in UI filtering. Later half-filled matches (`wb-2-*` waiting for feeders) must remain `Pending`.
- **`isBye` flag as display source of truth** — Keep internal 1–0 scores for advancement; only presentation changes. Default `isBye = false` preserves backward compatibility with persisted event replays (zio-json `DeriveJsonCodec` picks up the default).
- **Single display helper** — Add `BracketLayout.resultLabel` in `liga-js`; both `BracketView` and `AudienceBracketView` call it. No duplicated score/bye logic.
- **R1 winners byes only** — Losers-bracket bye rules are out of scope unless a case surfaces during testing.

## Dependency Graph

```
liga/model/Types.scala (isBye field)
    │
    ├── liga/bracket/BracketGen.scala
    │       ├── narrow isByeMatch → wb-1-* only
    │       └── mark isBye = true after bye advance
    │               └── liga/test/BracketSpec.scala
    │
    └── liga-js/api/Models.scala (mirror isBye)
            │
            ├── liga-js/director/BracketLayout.scala (resultLabel)
            │       └── liga-js/test/BracketLayoutSpec.scala
            │
            ├── liga-js/director/BracketView.scala
            ├── liga-js/audience/AudienceBracketView.scala
            ├── liga-js/director/MatchPanel.scala
            │
            ├── liga/src/main/resources/liga/web/css/director.css (.match-bye)
            └── liga-js/audience/AudienceApp.scala (.match-bye)
```

## Current State

| Component | Status |
|-----------|--------|
| `BracketGen.isByeMatch` | Matches **any** half-filled incomplete match (`hasA ^ hasB`) — causes cascade |
| `BracketMatch` | No `isBye` field |
| `BracketView` / `AudienceBracketView` | Inline `s"${result.scoreA}–${result.scoreB}"` with class `match-score` |
| `MatchPanel` (completed) | Shows `Final score: 1–0` for all completed matches |
| `BracketSpec` | Has 3-in-4 and 12-in-16 tests but no cascade-stop or `isBye` assertions |
| `director.css` | No `.match-score` or `.match-bye` rules (class used but unstyled) |
| `AudienceApp.scala` | Has `.match-score` inline CSS; no `.match-bye` |

## Task List

### Phase 1: Data Layer (liga)

- [ ] Task 1: Add `isBye` to domain model
- [ ] Task 2: Fix cascade in `BracketGen` and add `BracketSpec` tests

### Checkpoint: Data Layer

- [ ] `sbt --client "liga/testOnly ph.samson.atbp.liga.bracket.BracketSpec"` passes
- [ ] 3-in-4: P1 in one pending `wb-2-*`, no later cascade
- [ ] 12-in-16: four R1 byes with `isBye = true`, each winner in one pending `wb-2-*`
- [ ] 8-in-8: no matches have `isBye = true`
- [ ] Review with human before UI work

### Phase 2: Presentation Layer (liga-js)

- [ ] Task 3: Mirror `isBye` on JS model + `resultLabel` helper and tests
- [ ] Task 4: Update bracket views, match panel, and CSS

### Checkpoint: Complete

- [ ] `sbt --client "liga/test"` and `sbt --client "liga-js/test"` pass
- [ ] `sbt --client fixup` exits successfully with clean working tree
- [ ] Manual: 12-in-16 bracket shows `bye` on R1 top-seed rows (director + audience)
- [ ] Ready for review

---

## Task 1: Add `isBye` to domain model

**Description:** Add `isBye: Boolean = false` to `BracketMatch` in `liga/model/Types.scala`. This is the foundation for marking and displaying bye matches. The default preserves compatibility with existing persisted tournament events.

**Acceptance criteria:**
- [ ] `BracketMatch` has `isBye: Boolean = false` after `result`
- [ ] Project compiles (`sbt --client compile`)

**Verification:**
- [ ] `sbt --client compile` succeeds

**Dependencies:** None

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/model/Types.scala`

**Estimated scope:** XS (1 file)

---

## Task 2: Fix cascade in `BracketGen` and add `BracketSpec` tests

**Description:** Narrow `isByeMatch` to first-round winners bracket only (`wb-1-*` with XOR player slots). In `propagateByes`, set `isBye = true` on the match just completed. Extend `BracketSpec` with cascade-stop and `isBye` assertions for 3-in-4, 12-in-16, and 8-in-8.

**Acceptance criteria:**
- [ ] `isByeMatch` returns true only for `wb-1-*` matches that are incomplete with exactly one player
- [ ] After bye propagation, each bye winner appears in exactly one `wb-2-*` match in `Pending` state (not cascaded to later rounds)
- [ ] Completed R1 bye matches have `isBye = true` and internal 1–0 result
- [ ] Full 8-in-8 bracket has no `isBye = true` matches after generation
- [ ] Existing `BracketSpec` tests still pass

**Verification:**
- [ ] `sbt --client "liga/testOnly ph.samson.atbp.liga.bracket.BracketSpec"`

**Dependencies:** Task 1

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/bracket/BracketGen.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/bracket/BracketSpec.scala`

**Estimated scope:** S (2 files)

---

## Task 3: Mirror `isBye` on JS model + `resultLabel` helper and tests

**Description:** Add `isBye: Boolean = false` to `liga-js/api/Models.BracketMatch` so JSON from the server round-trips correctly. Add `BracketLayout.resultLabel` that returns `Some("bye")` for bye matches, `Some("N–M")` for normal completed matches, and `None` for pending matches. Add unit tests in `BracketLayoutSpec`.

**Acceptance criteria:**
- [ ] `liga-js` `BracketMatch` mirrors `isBye` field with default `false`
- [ ] `resultLabel` returns `Some("bye")` when `isBye = true` and result present
- [ ] `resultLabel` returns `Some("7–4")` for normal completed match
- [ ] `resultLabel` returns `None` for pending match (no result)

**Verification:**
- [ ] `sbt --client "liga-js/testOnly ph.samson.atbp.liga.js.director.BracketLayoutSpec"`

**Dependencies:** Task 1

**Files likely touched:**
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/api/Models.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/BracketLayout.scala`
- `liga-js/src/test/scala/ph/samson/atbp/liga/js/director/BracketLayoutSpec.scala`

**Estimated scope:** S (3 files)

---

## Task 4: Update bracket views, match panel, and CSS

**Description:** Replace inline score formatting in `BracketView` and `AudienceBracketView` with `BracketLayout.resultLabel`, using class `match-bye` vs `match-score`. Update `MatchPanel` completed state to show "Bye — auto-advance" for bye matches. Add `.match-bye` CSS to `director.css` and `AudienceApp.scala` (muted + italic per spec).

**Acceptance criteria:**
- [ ] Director bracket rows show `bye` (not `1–0`) for completed R1 bye matches
- [ ] Audience bracket rows show `bye` for completed R1 bye matches
- [ ] Match panel shows "Bye — auto-advance" for completed bye matches
- [ ] Normal completed matches still show `scoreA–scoreB` everywhere
- [ ] `.match-bye` styled: `font-style: italic; opacity: 0.75;` on both surfaces

**Verification:**
- [ ] `sbt --client "liga-js/test"`
- [ ] Manual check: seed 12-player tournament, confirm R1 bye rows show `bye` on director and audience

**Dependencies:** Task 2, Task 3

**Files likely touched:**
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/BracketView.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/audience/AudienceBracketView.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/MatchPanel.scala`
- `liga/src/main/resources/liga/web/css/director.css`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/audience/AudienceApp.scala`

**Estimated scope:** M (5 files)

---

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Persisted events lack `isBye` field | Low | Default `false`; old replays show scores until re-seeded |
| Losers-bracket partial-fill edge case | Med | Out of scope per spec; add test if discovered |
| `DeriveJsonCodec` breaks on new field | Low | Default parameter handles missing JSON key |
| Existing 3-in-4 / 12-in-16 tests too weak | Med | Strengthen with `wb-2-*` Pending + no cascade assertions in Task 2 |

## Open Questions

- None blocking — spec decisions are resolved (panel copy: "Bye — auto-advance"; styling: muted + italic).

## Parallelization Opportunities

- **Task 3** can start once Task 1 lands (does not need Task 2's BracketGen logic — `resultLabel` tests use hand-built `BracketMatch` values).
- **Task 4** requires both Task 2 (real `isBye` data from server) and Task 3 (`resultLabel` helper).
- Tasks 1 → 2 must be sequential. Tasks 2 and 3 can run in parallel after Task 1.
