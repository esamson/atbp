# Implementation Plan: D2 diagram support in md2c

Source spec: [SPEC.md](../SPEC.md) (commit `ba30630`). All eight resolved
decisions there are treated as settled; this plan only orders the work and
fills the implementation gaps the spec left open (see *Plan-level decisions*).

## Overview

Add `D2.scala` to md2c so `d2*` fences render to PNG/SVG during staging via the
`d2` CLI, presented exactly as Mermaid fences are — `MediaSingle`/
`ExternalMedia` on success, a preserved `CodeBlock` plus a `text` details
sibling on any failure. The ~50 lines of reflection-based sibling insertion
currently private to `Mermaid.scala` move to a shared `DiagramFailure.scala`
first, so D2 consumes it rather than duplicating it. The pinned `d2` v0.9.0
binary is installed into the published container image in the same change, because
with no runtime version check that pin is the only compatibility guarantee.

Seven tasks, five phases. The riskiest work (the subprocess contract) lands in
Task 2; the only task that cannot be verified on a machine without `d2` is
Task 4, and the only one needing a running podman VM is Task 6.

## Dependency graph

```
Task 1  DiagramFailure extraction         (gate: MermaidSpec unchanged)
   │
   ▼
Task 2  D2.scala + StagedTree wiring      ← the vertical slice
   │       + d2Available guard
   │       + 2 tests: success PNG, missing binary
   ├─────────────┬───────────────┐
   ▼             ▼               ▼
Task 3        Task 4          Task 5
Group A       Group B         staging fixtures
rest +        format          trees/D2 Diagram
ParserSpec    variants        trees/D2 Failure
   └─────────────┴───────────────┘
                 │
                 ▼
             Task 7  .cursor/rules/md2c.mdc

Task 6  d2 install step    — independent of all Scala tasks
        (build.sbt + project/Dependencies.scala)
```

Tasks 3, 4 and 5 are mutually independent once Task 2 lands. Task 6 shares no
files with any other task. Task 7 documents final behavior, so it goes last.

## Architecture decisions (from the spec)

- **Subprocess, not a library.** No JVM D2 library exists; D2 is Go. `d2` is
  found on `PATH`, making this md2c's first native-binary dependency — hence
  the missing-binary path is a first-class tested behavior.
- **Success is exit code 0 + non-empty output**, never "stderr was empty":
  verified locally, `d2` prints `success: successfully compiled …` to stderr on
  every successful run.
- **`(source, format)` render key**, so `d2` and `d2.svg` fences with identical
  bodies do not collide in the render map.
- **Only `IOException` from process start is caught** (→ `NotOnPathMessage`);
  every other exception fails the `Task`, matching Mermaid's rule.
- **No version check.** The image pin is the guarantee; a bad native install
  surfaces d2's own stderr through the soft-failure path.
- **CI does not install d2.** Group B is `whenZIO`-guarded and reports as
  ignored. `github.sbt` / `ci.yml` are untouched, keeping `githubWorkflowCheck`
  out of the picture.

## Plan-level decisions

The spec's invocation contract leaves four mechanics unstated. These are the
choices this plan commits to; each is a normal implementation detail, not a
reopening of a resolved decision. Flagged here so they are not re-litigated
mid-implementation.

### 1. Redirect stdout straight to the output file (not a pipe)

`ProcessBuilder.redirectOutput(outFile)` and `redirectError(errFile)` in the
per-document temp dir, stdin written from the fence source and closed.

**Why, concretely:** with a piped stdout, calling `waitFor()` before draining
the pipe deadlocks once the image exceeds the OS pipe buffer (64KB on Linux).
A locally rendered two-node PNG is already 10,143 bytes; a real diagram clears
64KB easily. Redirecting to a file removes the ordering hazard entirely and
makes the spec's "stdout bytes are the image" literal: success becomes
`exitCode == 0 && outFile.nonEmpty`.

### 2. The timeout is `Process.waitFor(30, SECONDS)`, inside the blocking block

The spec says "`ZIO` timeout wraps the process so a wedged binary cannot hang a
publish". A ZIO `.timeout` around `ZIO.attemptBlocking` does not deliver that:
the blocking region is uninterruptible, so the fiber's timeout would return
while the process ran on. `Process.waitFor(timeout)` returning `false` →
`destroyForcibly()` → soft failure achieves the stated intent reliably.

30 seconds, generous by two orders of magnitude against the ~6ms SVG / ~67ms
PNG measured locally, so it only ever fires on a genuinely wedged binary.

### 3. A timeout is a soft failure, not a `Task` failure

Details text: `Could not render diagram: d2 timed out after 30s`. The spec
never says what a timeout looks like to the reader, but the boundary "never let
a diagram failure of any kind fail the publish" settles it. Same
`RenderFailureDetailsHeader`, no special case — consistent with decision 6.

### 4. Empty stderr on a non-zero exit gets an exit-code fallback

`d2 exited with code $n` — the spec calls for "a fallback naming the exit
code" without fixing the wording.

## Task List

### Phase 1: Foundation

## Task 1: Extract sibling insertion into `DiagramFailure.scala`

**Description:** Move `insertAfterParentIndex` and `insertFailureSiblings` out
of `Mermaid.scala` into a new `DiagramFailure.scala`, parameterized by the
details header string so both renderers can call it. Pure move — no behavior
change, no new logic. Doing this before any D2 code exists means the extraction
is verified in isolation, with `MermaidSpec` as the only thing that can fail.

**Acceptance criteria:**
- [ ] `DiagramFailure.insertFailureSiblings(adf, errorByBlock, detailsHeader)`
      exists; `Mermaid.scala` calls it and no longer defines either helper.
- [ ] `Mermaid.RenderFailureComment` / `RenderFailureDetailsHeader` stay on
      `Mermaid` (public API used by `MermaidSpec` assertions).
- [ ] Zero edits to any `MermaidSpec` assertion.

**Verification:**
- [ ] `sbt --client "md2c/testOnly *MermaidSpec*"` passes — this is the gate.
- [ ] `sbt --client "md2c/test"` passes.
- [ ] `git diff` on `MermaidSpec.scala` is empty.

**Fallback:** if the extraction turns out less contained than it looks (the
reflection path is sensitive to the `AbstractContentNode` shape), abandon it,
leave `Mermaid.scala` untouched, and duplicate the two helpers privately in
`D2.scala`. The spec explicitly permits this; note the decision in the commit.

**Dependencies:** None

**Files likely touched:**
- `md2c/src/main/scala/ph/samson/atbp/md2c/DiagramFailure.scala` (new)
- `md2c/src/main/scala/ph/samson/atbp/md2c/Mermaid.scala`

**Estimated scope:** S (2 files)

### Checkpoint: Foundation
- [ ] `sbt --client "md2c/test"` green
- [ ] `MermaidSpec` and `ParserSpec` untouched
- [ ] Commit per the `AGENTS.md` loop (`git add -A` → `sbt --client fixup` →
      `git status`, repeat until both clean in the same check)

### Phase 2: Core vertical slice

## Task 2: `D2.scala` — render, transform, pipeline wiring

**Description:** The whole vertical path in one task: a `d2` fence in Markdown
→ subprocess → image file → `MediaSingle`/`ExternalMedia` in the staged doc,
plus the missing-binary soft failure. Both branches of `RenderOutcome` are live
from the start; the two tests added here prove one branch each. Follows
`Mermaid.scala`'s shape and the object skeleton given in the spec's *Code
Style* section.

Implements: `render(adf, executable = "d2")` (`private[md2c]` executable
parameter — the seam the missing-binary test uses), `transform(adf)`,
`isD2`, `formatFromLanguage`, `renderKey`. One
`File.newTemporaryDirectory()` per document, outputs named
`fig-${index + 1}.d2.$format`, `ZIO.collectAllPar` over the fence list.
Invocation `d2 --stdout-format <png|svg> - -` with the redirect and timeout
mechanics from *Plan-level decisions*. Then one line in `StagedTree.convert`:
`d2Rendered <- D2.transform(mermaidRendered)`, after Mermaid and before
Extensions.

Comment the two non-obvious things, as the spec asks: why exit code and not
stderr decides success, and why there is no version check.

**Acceptance criteria:**
- [ ] A ` ```d2 ` fence transforms to `ExternalMedia` at a non-empty
      `.d2.png` path; the `d2` `CodeBlock` is gone (AC 1).
- [ ] `render` with a nonexistent executable name yields
      `Left("Could not render diagram: d2 not on PATH")`, and `transform`
      keeps the `CodeBlock` with a `text` sibling carrying that message
      under `RenderFailureDetailsHeader` (AC 5).
- [ ] `d2Available: UIO[Boolean]` probes the binary once and never fails;
      the success test is inside a suite carrying `.whenZIO(d2Available)`.

**Verification:**
- [ ] `sbt --client "md2c/testOnly *D2Spec*"` passes with **zero ignored** on
      this machine (`d2` v0.9.0 is on `PATH`, confirmed).
- [ ] Success-path assertion decodes the PNG and checks
      `width > 8 && height > 8`, guarding a degenerate raster (same guard
      `MermaidSpec` uses against Batik blanks).
- [ ] `sbt --client "md2c/test"` passes — `MermaidSpec`, `ParserSpec`,
      `StagedTreeSpec` unaffected by the pipeline line.
- [ ] Verify the timeout path is reachable by temporarily pointing the
      executable at a script that sleeps past the limit; confirm a soft
      failure, not a hang and not a `Task` failure. Revert the probe.

**Dependencies:** Task 1

**Files likely touched:**
- `md2c/src/main/scala/ph/samson/atbp/md2c/D2.scala` (new)
- `md2c/src/main/scala/ph/samson/atbp/md2c/StagedTree.scala`
- `md2c/src/test/scala/ph/samson/atbp/md2c/D2Spec.scala` (new)

**Estimated scope:** M (3 files, one of them the bulk of the feature)

## Task 3: Group A — remaining failure-path and parser coverage

**Description:** Fill out the tests that need no `d2` binary, so CI covers the
whole failure side. All are driven through the missing-binary seam from Task 2.
Adds the parser fixture and its `ParserSpec` case alongside the existing
mermaid one.

**Acceptance criteria:**
- [ ] Failure keeps the `CodeBlock` with its original language (`d2`,
      `d2.png`, `d2.svg` all asserted), body
      `RenderFailureComment` + `\n` + original source, no `ExternalMedia`
      (AC 4 structure).
- [ ] Failure sibling is a `CodeBlock` with language `text` whose body starts
      with `RenderFailureDetailsHeader`.
- [ ] A failing `d2` block nested in a `Panel` puts the sibling inside the
      panel — `panel.allNodesOfType(classOf[CodeBlock]).count() == 2`, not at
      doc top level.
- [ ] Non-d2 `CodeBlock`s (`scala`, `plantuml`, `mermaid`) pass through
      unchanged with no `ExternalMedia` (AC 6).
- [ ] `ParserSpec` has a `d2/Shapes.md` case asserting `doc.isSupported`.

**Verification:**
- [ ] `sbt --client "md2c/testOnly *D2Spec*"` passes.
- [ ] `sbt --client "md2c/testOnly *ParserSpec*"` passes.

**Dependencies:** Task 2

**Files likely touched:**
- `md2c/src/test/scala/ph/samson/atbp/md2c/D2Spec.scala`
- `md2c/src/test/scala/ph/samson/atbp/md2c/ParserSpec.scala`
- `md2c/src/test/resources/markdown/d2/Shapes.md` (new)

**Estimated scope:** S (3 files, tests only)

## Task 4: Group B — format variants and cache separation

**Description:** The remaining tests that need a real `d2`, inside the
`whenZIO(d2Available)` suite from Task 2.

**Acceptance criteria:**
- [ ] ` ```d2.png ` renders a non-empty `.d2.png` (AC 2).
- [ ] ` ```d2.svg ` renders a non-empty `.d2.svg` whose contents contain
      `<svg` (AC 2).
- [ ] The same source in a `d2` fence and a `d2.svg` fence on one page yields
      two files at distinct paths with distinct formats (AC 3) — the direct
      test of the `(source, format)` render key.

**Verification:**
- [ ] `sbt --client "md2c/testOnly *D2Spec*"` — zero ignored, all green.
- [ ] Force `d2Available` to `ZIO.succeed(false)` and re-run: Group B reports
      as **ignored, not failed**, Group A stays green, the run exits 0. Revert
      the forcing. This is the shape CI will see and is a named success
      criterion in the spec.

**Dependencies:** Task 2

**Files likely touched:**
- `md2c/src/test/scala/ph/samson/atbp/md2c/D2Spec.scala`

**Estimated scope:** XS-S (1 file)

### Checkpoint: Core slice complete
- [ ] `sbt --client "md2c/test"` green; `D2Spec` zero ignored locally
- [ ] Forced-`false` run: Group B ignored, Group A green, exit 0
- [ ] Every acceptance criterion except AC 7 (the image) has a named test
- [ ] No edits to `MermaidSpec` assertions
- [ ] `AGENTS.md` commit loop clean; **review with human before Phase 3**

### Phase 3: Staging fixtures

## Task 5: End-to-end staging fixtures

**Description:** Two fixture trees exercising `StagedTree.from`, mirroring
`trees/Mermaid Sequence` and `trees/Mermaid Failure`. `D2 Diagram` carries d2,
plantuml and mermaid fences on one page, proving the three renderers coexist in
the pipeline. `D2 Failure` carries invalid d2 source, proving staging still
succeeds and that the details text is d2's own stderr — the only place the real
error text is asserted. Both are Group B (the diagram tree needs the binary;
the failure tree needs it to produce genuine stderr).

**Acceptance criteria:**
- [ ] `trees/D2 Diagram/{.md2c.conf, D2 Diagram.md}` stages to a doc with no
      `d2*`/`plantuml*`/`mermaid*` languages left, and `ExternalMedia` count
      matching its d2 + plantuml + mermaid fence count (AC 6).
- [ ] `trees/D2 Failure/{.md2c.conf, D2 Failure.md}` stages with no `Task`
      failure, yielding one `d2` `CodeBlock` plus a `text` sibling whose body
      is `RenderFailureDetailsHeader` + `\n` + d2's stderr, and no
      `ExternalMedia` (AC 4 details text).
- [ ] `.md2c.conf` files contain `spaceId = 1234567`, matching the existing
      fixtures.

**Verification:**
- [ ] `sbt --client "md2c/testOnly *D2Spec*"` passes.
- [ ] `sbt --client "md2c/testOnly *StagedTreeSpec*"` passes — the new trees
      must not disturb its expectations.
- [ ] `sbt --client "md2c/test"` passes.

**Dependencies:** Task 2

**Files likely touched:**
- `md2c/src/test/resources/trees/D2 Diagram/.md2c.conf` (new)
- `md2c/src/test/resources/trees/D2 Diagram/D2 Diagram.md` (new)
- `md2c/src/test/resources/trees/D2 Failure/.md2c.conf` (new)
- `md2c/src/test/resources/trees/D2 Failure/D2 Failure.md` (new)
- `md2c/src/test/scala/ph/samson/atbp/md2c/D2Spec.scala`

**Estimated scope:** M (5 files, all small)

### Phase 4: Distribution

## Task 6: Install pinned `d2` v0.9.0 in the container image

**Container engine: podman, always.** There is no `docker` binary on this
machine at all (`podman` 5.8.2 at `/opt/podman/bin/podman`). Every command
below is podman. The sbt task and setting names still read `Docker` /
`docker*` — that is sbt-native-packager's naming, not a tool choice, and those
identifiers must be typed as the plugin spells them.

**Description:** Add `Versions.D2 = "v0.9.0"` to `project/Dependencies.scala`
as the single source of truth, and a second `Cmd("RUN", …)` in the `cli`
project's `dockerCommands` that fetches the pinned release tarball, verifies it
against the release's published `SHA256SUMS`, installs the binary, and cleans up
the tarball in the same layer. Arch comes from `dpkg --print-architecture`
(`amd64`/`arm64`) so the step stays correct if the image ever goes multi-arch.
A checksum mismatch must fail the build.

Confirmed against the real release: assets `d2-v0.9.0-linux-amd64.tar.gz` /
`…-arm64.tar.gz` and a `SHA256SUMS` in standard `sha256sum -c` format
(`<sha>  <filename>`) both exist.

### Pointing sbt-native-packager at podman

`dockerBuildCommand` defaults to `dockerExecCommand ++ Seq("build", …)`, and
`dockerExecCommand` defaults to `Seq("docker")` — confirmed in the 1.11.7
sources (`docker/Keys.scala:41`, `DockerPlugin.scala:175`). With no `docker`
binary present, `Docker / publishLocal` fails until that is overridden.
`dockerExecCommand` also feeds `dockerVersion`, `dockerApiVersion` and
`dockerRmiCommand`, so one setting covers the whole plugin.

**Recommended: `local.sbt`**, which is already in `.gitignore`:

```scala
dockerExecCommand := Seq("podman")
```

Local-only, zero repo footprint, and the spec's own command
(`sbt --batch "cli / Docker / publishLocal"`) then works verbatim.

**Fallback if that misbehaves:** skip the plugin's build entirely —
`sbt --batch "cli / Docker / stage"` writes the generated Dockerfile plus its
context under `cli/target/docker/stage/`, then `podman build` that directory
directly. Needs no sbt setting at all and verifies the exact generated
Dockerfile.

Do **not** commit `dockerExecCommand := Seq("podman")` to `build.sbt` as part
of this task — CI publishes through `dockerPublish` after a `docker login` in
`github.sbt:31`, and the spec froze `github.sbt` / `ci.yml`. Switching the
repo's own engine is a separate change (see *Open Questions*).

**Acceptance criteria:**
- [ ] `Versions.D2` is the only place the version string appears — verified by
      `grep -rn "0\.9\.0" build.sbt project/`.
- [ ] `podman run --rm --entrypoint d2 <local image> --version` reports
      `v0.9.0` (AC 7).
- [ ] Rendering works in the image with no host setup: a `d2` fence staged
      inside the container produces a PNG (browser-free PNG is exactly what
      the v0.9.0 pin buys).
- [ ] Perturbing the expected sum once makes the image build **fail**.
- [ ] No change to `github.sbt` or `.github/workflows/ci.yml`.

**Verification:**
- [ ] `sbt --batch "cli / Docker / publishLocal"` succeeds — `--batch` because
      this task edits build files, per `AGENTS.md`.
- [ ] `podman run --rm --entrypoint d2 ghcr.io/esamson/atbp:latest --version`
- [ ] Perturbed-checksum build fails; restore and rebuild clean.
- [ ] `sbt --batch "githubWorkflowCheck"` still passes.

**Prerequisites (all currently unmet on this machine):**
- **The podman VM is down.** `podman machine list` shows
  `podman-machine-default` last up 4 days ago. `podman machine start` first.
- `dockerExecCommand := Seq("podman")` in `local.sbt` (above) — without it the
  plugin shells out to a `docker` binary that does not exist here.
- `curl` presence in `eclipse-temurin:25-jre-noble` is unverified. First step
  of the task: `podman run --rm eclipse-temurin:25-jre-noble sh -c 'command -v
  curl wget'`. If neither is present, add `curl ca-certificates` to the
  **existing** `git graphviz` apt step rather than introducing a third layer.
- The tarball's internal layout is unverified. Run `tar tzf` on the downloaded
  asset to locate the binary before writing the install line; d2's tarballs
  ship a `Makefile`/`scripts` tree alongside the binary, so a bare
  `tar -xzf -C /usr/local/bin` will not do.

**Watch for:** `dockerVersion` runs `<engine> version --format
'{{.Server.Version}}'` and native-packager parses the result to decide on
buildkit and `--platform` flags. Podman answers that format, but if the parse
comes back empty or odd, the `Docker / stage` + `podman build` fallback
sidesteps it.

**Dependencies:** None (touches no Scala source). Independent of Tasks 1-5.

**Files likely touched:**
- `project/Dependencies.scala`
- `build.sbt`
- `local.sbt` (new, gitignored — not part of any commit)

**Estimated scope:** S (2 committed files, but the verification loop is the
slow part)

### Checkpoint: Distribution
- [ ] Image renders a d2 fence with no host setup
- [ ] Checksum failure demonstrated once, then reverted
- [ ] `githubWorkflowCheck` still satisfied

### Phase 5: Documentation

## Task 7: Update `.cursor/rules/md2c.mdc`

**Description:** Document the shipped behavior in the md2c rules file: the
stack line gains D2, the conventions gain the d2 fences and their soft-failure
format, and the boundaries gain the `d2`-on-`PATH` requirement plus the
deliberate absence of a version check.

**Acceptance criteria:**
- [ ] Stack section names D2 (`d2` CLI) alongside PlantUML and Mermaid.
- [ ] Conventions list the fences (`d2`, `d2.png`, `d2.svg`), the success →
      `MediaSingle` path, and the soft-failure format verbatim
      (`# D2 rendering failed. See details below.` /
      `D2 failure details:`), matching how the Mermaid entries read.
- [ ] Records that `d2` must be on `PATH`, that there is **no** version check,
      and that the container image pins the version.
- [ ] Commands section lists `sbt --client "md2c/testOnly *D2Spec*"`.
- [ ] Notes that Group B is skipped without `d2` and shows up as ignored.

**Verification:**
- [ ] Manual read-through against the final `D2.scala` — every documented
      string matches a constant in the code.

**Dependencies:** Tasks 1-6 (documents final behavior)

**Files likely touched:**
- `.cursor/rules/md2c.mdc`

**Estimated scope:** XS (1 file)

### Checkpoint: Complete
- [ ] All seven acceptance criteria from the spec met (AC 1-6 by test, AC 7 by
      the image check)
- [ ] Every box in the spec's *Success Criteria* section ticked
- [ ] `sbt --client fixup && git status` leaves a clean tree in the same check
- [ ] Ready for review

## Commit strategy

Per `AGENTS.md`, every commit touching Scala or the build runs the full loop —
`git add -A` (new files included; untracked sources are not formatted until
they are in the tree) → `sbt --client fixup` → `git status` → repeat until
`fixup` exits clean **and** the tree is clean **in the same check** → one
commit carrying both the change and its formatting.

One commit per task (decided). Task 1 must be its own commit regardless — a
pure refactor mixed into the feature commit destroys the `MermaidSpec`-green
audit trail. Task 6 is its own commit (build definition; `--batch` for the sbt
run that follows the edit). Task 7 is its own commit (docs).

## Risks and Mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| `DiagramFailure` extraction breaks Mermaid's reflection path | Med | Task 1 is standalone with `MermaidSpec` as the gate; documented fallback is to duplicate in `D2.scala` |
| Pipe deadlock on images >64KB | High — hangs a publish | Redirect stdout to a file; never `waitFor` on an undrained pipe (*Plan-level decisions* #1) |
| ZIO timeout does not actually kill a wedged `d2` | Med | Timeout via `Process.waitFor(30, SECONDS)` + `destroyForcibly()` inside the blocking block, not a fiber-level `.timeout` (#2) |
| CI never renders a D2 diagram (spec decision 8, accepted) | Med | Group A covers failure path, ADF shape and parser on every PR; the ignored count is the visible signal that Group B did not run — never suppress it |
| The image pin is the only compatibility guarantee (spec decisions 5+7, accepted) | Med | Pre-v0.6.9 → unsupported-flag soft failure; v0.6.9-v0.8.x → possible Chromium prompt. Both surface as d2's stderr on the page, not a crash |
| Base image may lack `curl` | Low | First step of Task 6 probes it; fold into the existing apt step if missing |
| No `docker` binary here; podman VM is down | Low | Task 6 uses podman throughout: `podman machine start` plus `dockerExecCommand := Seq("podman")` in gitignored `local.sbt`, both named as prerequisites |

## Settled before implementation

Answered 2026-09-09; recorded so they are not reopened.

1. **Commit granularity: one commit per task**, seven in total. Task 1 must be
   its own commit regardless (see *Commit strategy*).
2. **The repo keeps docker for GitHub publishing.** `dockerPublish` and the
   `docker login ghcr.io` step in `github.sbt:31` stay exactly as they are —
   the podman override lives only in gitignored `local.sbt`, for local work on
   this machine. Do not touch `github.sbt` or `ci.yml`, matching spec
   decision 8.
3. **`tasks/` is committed** to version control alongside the work.

## Open Questions

None. Ready for `/build`.
