# Spec: Liga minimum 3-player tournaments

## Objective

Liga currently requires **8–64 players** to lock a roster and seed a
double-elimination bracket. Small club nights and ad-hoc events often have only
3–7 players, so directors cannot run a tournament through the normal wizard.

**Goal:** Allow tournaments with **3–64 players**, using the same
double-elimination model as today. Small fields get a smaller bracket with byes
(e.g. 3 players → 4-slot bracket, lowest seed receives a bye).

**Users:** Tournament directors using the liga serve UI (`liga-js` director
wizard) or the HTTP API / event replay path.

**User stories:**

- As a director, I can lock a roster of 3 players and proceed through
  race-to configuration and seeding.
- As a director, I see guidance that reflects the new 3–64 range when my
  roster is too small or too large.
- As a developer replaying event logs, a `PlayersLocked` event with 3–7 players
  replays without validation errors.

**Acceptance criteria:**

| Count | Lock roster | Seed bracket | Bracket size |
|------:|:-----------:|:------------:|:------------:|
| 1–2   | Reject      | —            | —            |
| 3     | Accept      | Accept       | 4 (1 bye)    |
| 4     | Accept      | Accept       | 4            |
| 5–7   | Accept      | Accept       | 8 (byes)     |
| 8–64  | Accept      | Accept       | unchanged    |
| 65+   | Reject      | —            | —            |

## Tech Stack

- **Language:** Scala 3.8.4
- **Test framework:** ZIO Test
- **Modules touched:**
  - `liga` — tournament commands, seeding, bracket generation, replay
  - `liga-common` — `RaceToScopes` (shared JVM + JS)
  - `liga-js` — director wizard copy (`DirectorGuidance`)

No new dependencies. No database or schema changes (event-sourced JSON on disk).

## Commands

```bash
# Compile all modules
sbt --client compile

# Run liga unit tests
sbt --client "liga/test"

# Run liga-common tests (RaceToWizard, etc.)
sbt --client "ligaCommonJVM/test"

# Run targeted bracket / tournament tests
sbt --client "liga/testOnly *Bracket*"
sbt --client "liga/testOnly *Tournament*"
sbt --client "liga/testOnly *RaceToScopes*"

# Format + compile gate (required before commit)
sbt --client fixup
```

## Project Structure

```
liga/
  src/main/scala/ph/samson/atbp/liga/
    bracket/
      Seeding.scala          # bracketSize — add support for 4-slot brackets
    tournament/
      Tournament.scala       # lockPlayers validation
      Seed.scala             # seed-time player count validation
      Replay.scala           # replay validation for PlayersLocked
      TournamentValidation.scala  # (optional) centralize player-count check
  src/test/scala/ph/samson/atbp/liga/
    bracket/
      BracketSpec.scala      # add 3–4 player cases
      RaceToScopesTopologySpec.scala  # extend range 3..64
    tournament/
      TournamentSpec.scala   # lockPlayers 3-player success, 1–2 reject

liga-common/
  src/main/scala/ph/samson/atbp/liga/bracket/
    RaceToScopes.scala       # bracketSize for 3–7 players

liga-js/
  src/main/scala/ph/samson/atbp/liga/js/director/
    DirectorGuidance.scala   # lockRosterHint + friendlyApiError copy
```

## Code Style

Follow existing liga conventions: pure command handlers returning
`Either[Error, Event]`, validation messages as stable strings (replay depends on
them), bracket sizing as next power of two.

Introduce a single source of truth for roster bounds in `liga-common` so JVM
and JS stay aligned:

```scala
// liga-common — illustrative; exact module placement TBD in plan
object TournamentBounds {
  val MinPlayers: Int = 3
  val MaxPlayers: Int = 64

  def validPlayerCount(count: Int): Boolean =
    count >= MinPlayers && count <= MaxPlayers

  /** Smallest power-of-two bracket that fits count players (4, 8, 16, 32, 64). */
  def bracketSize(playerCount: Int): Int = ???
}
```

`Seeding.bracketSize` and `RaceToScopes` private `bracketSize` should delegate
to this shared logic (or one should call the other) to prevent drift.

Error messages should use a consistent range string, e.g.
`player count must be 3–64`.

## Testing Strategy

**Framework:** ZIO Test (`assertTrue`, `suite`, `test`).

**Levels:**

| Concern | Where | Examples |
|---------|-------|----------|
| Bracket sizing & bye propagation | `BracketSpec` | 3 players → size-4 bracket, 6 matches; bye auto-advances |
| Scope keys vs topology | `RaceToScopesTopologySpec` | Loop `3 to 64`, assert `requiredKeys` matches `BracketTopology` |
| Wizard commands | `TournamentSpec` | `lockPlayers` accepts 3, rejects 2 |
| Seeding | `SeedSpec` / `ReplaySpec` | Seed 3-player tournament end-to-end |
| Director copy | `DirectorGuidance` tests if present, else add | Hint text for count 2 vs 3 |

**Coverage expectation:** Every validation site that currently checks `8..64`
gets a test proving the new boundary. At least one full wizard path for 3
players (lock → race-to → seed → bracket has expected match count).

**Not required for this change:** New end-to-end HTTP test with 3 players
(unless cheap to add); existing `EndToEndSpec` can stay at 8/16.

## Boundaries

**Always:**

- Run `sbt --client fixup` and confirm clean `git status` before committing
  Scala changes.
- Keep validation logic consistent across `Tournament`, `Seed`, `Replay`, and
  `Seeding`.
- Preserve double-elimination semantics; small brackets use byes, not a
  different tournament type.
- Update error-message string replacements in `DirectorGuidance` alongside
  server messages.

**Ask first:**

- Changing maximum player count (stays 64).
- Introducing an alternate bracket format for small fields.
- Breaking changes to persisted event JSON shape.

**Never:**

- Allow 1–2 player tournaments (confirmed out of scope).
- Remove or weaken existing 8–64 bracket tests; extend them.
- Duplicate `bracketSize` logic in three places without a shared definition.

## Success Criteria

1. `Seeding.bracketSize(3)` returns `4`; `Seeding.bracketSize(7)` returns `8`.
2. `BracketGen.generate` for 3 rated players produces a size-4 bracket with
   bye propagation (lowest seed or empty slot advanced).
3. `Tournament.lockPlayers` succeeds with 3 players and fails with 2.
4. `Seed.buildEvents` succeeds for a locked 3-player tournament with complete
   race-to scopes.
5. `Replay` accepts `PlayersLocked` replay for rosters of size 3–64.
6. `RaceToScopesTopologySpec` passes for all counts `3..64`.
7. Director UI shows "Need at least 3 players…" (not 8) when count &lt; 3.
8. All existing tests pass; new tests cover 3- and 4-player paths.

## Open Questions

None — confirmed with stakeholder:

- Minimum **3** players (1–2 rejected).
- Standard double-elimination with byes (3 → 4-slot bracket).
- Maximum **64** unchanged.

## Implementation notes (for planning phase)

**Files requiring validation message / bound updates (grep: `8–64`, `8 and 64`,
`< 8`):**

- `liga/.../tournament/Tournament.scala`
- `liga/.../tournament/Seed.scala`
- `liga/.../tournament/Replay.scala`
- `liga/.../bracket/Seeding.scala`
- `liga-common/.../bracket/RaceToScopes.scala`
- `liga-js/.../director/DirectorGuidance.scala`

**Bracket size mapping after change:**

| Players | Bracket size |
|--------:|:------------:|
| 3–4     | 4            |
| 5–8     | 8            |
| 9–16    | 16           |
| 17–32   | 32           |
| 33–64   | 64           |

**Risk:** `RaceToScopes` and `Seeding` each have independent `bracketSize`
implementations today. Consolidating into `liga-common` avoids subtle mismatch
for 3–7 players.
