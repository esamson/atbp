# Plan: Liga minimum 3-player tournaments

## Approach

1. Add `TournamentBounds` in `liga-common` as the single source of truth for
   roster limits (3–64) and bracket sizing (4, 8, 16, 32, 64).
2. Wire all validation and sizing call sites to `TournamentBounds`.
3. Extend tests for 3–4 player brackets and updated boundary checks.
4. Update director UI copy in `DirectorGuidance`.

## Component dependencies

```
TournamentBounds (liga-common)
    ├── RaceToScopes.requiredKeys
    ├── Seeding.bracketSize
    ├── Tournament.lockPlayers
    ├── Seed.validatePlayerCount
    ├── Replay (PlayersLocked)
    └── DirectorGuidance (hints + error translation)
```

## Implementation order

1. **TournamentBounds** — no downstream deps; enables everything else.
2. **RaceToScopes + Seeding** — bracket topology must be correct before seed tests.
3. **Tournament + Seed + Replay** — command/replay validation.
4. **DirectorGuidance** — UI copy.
5. **Tests** — bracket, topology, wizard, seed/replay.

## Risks

| Risk | Mitigation |
|------|------------|
| `Seeding` and `RaceToScopes` drift | Both delegate to `TournamentBounds.bracketSize` |
| Bye propagation breaks for size-4 | Explicit `BracketSpec` test for 3 players |
| Error string mismatch in JS | Centralize range in `TournamentBounds`; update `DirectorGuidance` |

## Verification checkpoints

- After step 2: `liga/testOnly *Bracket*` and `*RaceToScopes*`
- After step 3: `liga/testOnly *Tournament*` and `*Replay*`
- Final: `sbt --client "liga/test"` + `ligaCommonJVM/test`
