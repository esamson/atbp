# Tasks: Liga minimum 3-player tournaments

- [x] Task: Add `TournamentBounds` in liga-common
  - Acceptance: `validPlayerCount`, `bracketSize`, shared error message
  - Verify: `sbt --client "ligaCommonJVM/compile"`
  - Files: `liga-common/.../bracket/TournamentBounds.scala`

- [x] Task: Delegate `RaceToScopes` and `Seeding` to `TournamentBounds`
  - Acceptance: `bracketSize(3) == 4`, `bracketSize(7) == 8`
  - Verify: `sbt --client "liga/testOnly *Bracket*"`
  - Files: `RaceToScopes.scala`, `Seeding.scala`

- [x] Task: Update tournament validation (Tournament, Seed, Replay)
  - Acceptance: lock/seed/replay accept 3, reject 2
  - Verify: `sbt --client "liga/testOnly *Tournament*"`
  - Files: `Tournament.scala`, `Seed.scala`, `Replay.scala`, `TournamentValidation.scala`

- [x] Task: Update DirectorGuidance and WizardView copy
  - Acceptance: hints and error translation say 3–64
  - Verify: compile liga-js
  - Files: `DirectorGuidance.scala`, `WizardView.scala`

- [x] Task: Add/extend tests
  - Acceptance: BracketSpec 3-player case; TopologySpec 3..64; TournamentSpec 3-player lock; SeedSpec 3-player seed
  - Verify: `sbt --client "liga/test"` — 198 passed
  - Files: `BracketSpec.scala`, `RaceToScopesTopologySpec.scala`, `TournamentSpec.scala`, `SeedSpec.scala`
