# Bye bracket display fix

## Problem Statement

**How might we show bye-advanced players only in their immediate next match, matching the UX of a normal round-1 win, on both director and audience views?**

## Recommended Direction

Limit `BracketGen.propagateByes` to first-round winners bracket matches (`wb-1-*`) where one seed slot is empty. Complete those as 1–0 and place the winner in `wb-2-*` pending — then stop. Do not treat later half-filled matches (waiting for a feeder winner) as byes.

The UI needs no changes; both surfaces already render bracket state faithfully.

Root cause: `isByeMatch` treats any half-filled incomplete match as a bye. After a round-1 bye, the winner lands in `wb-2-X` alone — still half-filled — so `propagateByes` recursively auto-completes through every later winners round, writing 1–0 results at each hop.

## Key Assumptions to Validate

- [ ] R1 winners byes are the only structural empty slots in partial-fill brackets
- [ ] `wb-2-*` with one player should stay Pending until the feeder completes
- [ ] Existing R1 bye tests still pass with stricter downstream assertions

## MVP Scope

**In:**

- Narrow bye detection in `BracketGen` (first-round winners only)
- Tests: 3-in-4 and 12-in-16 — player in exactly one future match, not cascaded
- Assert wb-2 is Pending (not Completed) after bye propagation

**Out:**

- Display-layer filtering or deduplication
- Losers-bracket bye rules (unless a case surfaces)
- Changing how completed R1 byes render (keep 1–0, done)

## Not Doing (and Why)

- **UI-only fix** — root cause is data; display patch would leave API wrong
- **Full cascade then scrub** — fights Advancement, leaves garbage state
- **Deferred/lazy bye resolution** — same outcome, more complexity
- **Hiding R1 bye matches** — director wants them visible as completed

## Open Questions

- None blocking — ready to implement
