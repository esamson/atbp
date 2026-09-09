# Spec: D2 diagram support in md2c

## Objective

Render [D2](https://d2lang.com) fenced code blocks into images during md2c
staging, so Markdown trees published to Confluence show D2 diagrams as
attached media instead of raw source text.

**User:** anyone running `atbp md2c` over a Markdown tree that contains D2
source. Today `plantuml*` and `mermaid*` fences render; `d2` fences publish as
plain code blocks.

**Success looks like:** a `d2` fence in a source file becomes a full-width
image attachment on the Confluence page, indistinguishable in handling from an
existing `mermaid` fence.

### Acceptance criteria

1. A ` ```d2 ` fence renders to PNG and appears as a `MediaSingle` /
   `ExternalMedia` node pointing at a non-empty `.d2.png` file.
2. ` ```d2.png ` renders PNG; ` ```d2.svg ` renders SVG (`.d2.svg`, non-empty,
   contains `<svg`).
3. The same source in a `d2` fence and a `d2.svg` fence on one page produces
   two files with distinct formats (no cache collision).
4. Invalid D2 source does **not** fail staging. The page still publishes, with
   the fence kept as a code block plus a sibling details block (see
   *Soft-failure presentation*).
5. `d2` missing from `PATH` produces the same soft-failure presentation with
   the message `Could not render diagram: d2 not on PATH`.
6. Non-D2 code blocks, and existing PlantUML / Mermaid behavior, are unchanged
   — `MermaidSpec` and `ParserSpec` stay green without edits to their
   assertions.
7. The published Docker image can render `d2` fences with no host setup.

### Non-goals

- Configurable theme / layout / sketch / padding. Fence suffix is the only
  knob; everything else is d2's default (dagre, theme 0, pad 100).
- Formats beyond PNG and SVG (d2 also emits PDF, PPTX, GIF, ASCII).
- Multi-board `--target` / animation support.
- Bundling or auto-installing the `d2` binary on developer machines.
- Any change to how PlantUML or Mermaid render.

## Tech Stack

- Scala 3.9.0, ZIO 2.1.26, better-files 3.9.2
- ADF via `adf-builder` (`adf-builder-java` 2.6.0)
- The `d2` CLI, invoked as an external process found on `PATH`. **No version
  check** — see *Version handling* below
- No new sbt dependency

### Why a subprocess (decision + evidence)

There is no JVM D2 library. D2 is Go; the only non-CLI paths are WASM builds
driven from JS (`@terrastruct/d2`, `d2wasm`), which would mean building a
`d2-java` sibling to `mermaid-java` — a separate library project, not a
feature. So md2c shells out. This makes D2 md2c's first native-binary
dependency (PlantUML is pure Java, mermaid-java is GraalJS), which is why the
missing-binary path is a first-class, tested behavior rather than a crash.

Verified locally against d2 v0.9.0:

| Behavior | Evidence |
|---|---|
| SVG via stdin→stdout | `cat x.d2 \| d2 --stdout-format svg - -` → valid SVG, ~6ms |
| PNG needs **no** browser | same with `png` → 11KB PNG in 67ms, no Playwright/Chromium cache on the machine |
| Errors are clean | invalid source → exit 1, **empty stdout**, `err: failed to compile -: -:2:1: connection missing destination` on stderr |
| stderr is noisy on success | prints `success: successfully compiled - to - in 5.5ms` — so **exit code, not stderr content, decides success** |

### Version handling

md2c does **not** check the d2 version. Two floors exist in d2's own history:

| Version | What it gates | Evidence |
|---|---|---|
| v0.6.9 | `--stdout-format` exists at all; below this our invocation fails outright | [0.6.9 release notes](https://d2lang.com/releases/0.6.9/) — flag "only used when output is set to stdout (`-`)" |
| v0.9.0 | browser-free **PNG** (SVG never needed a browser) | v0.9.0 changelog: "render PNG, GIF, PDF, and PPTX with the built-in renderer"; v0.8.2 changelog: "Chromium download through CLI for PNG exports is prompted" |

Neither is enforced in code, because in practice users get the version pinned
in the Docker image, and anyone running a native `d2` owns their install. An
unsupported flag or a Chromium-era PNG export fails the subprocess, and the
soft-failure path surfaces d2's own stderr — more informative than any version
message md2c could synthesize. The pinned image version is therefore the de
facto guarantee, which is what makes the Docker step part of this change
rather than a follow-up.

## Commands

```bash
sbt --client "md2c/compile"
sbt --client "md2c/test"
sbt --client "md2c/testOnly *D2Spec*"
sbt --client "md2c/testOnly *MermaidSpec*"   # regression guard
sbt --client fixup
```

`build.sbt` / `project/` are edited in this work (Docker commands), so use
`sbt --batch` for the run that follows those edits, per `AGENTS.md`; otherwise
`--client`.

Docker image check:

```bash
sbt --batch "cli / Docker / publishLocal"
docker run --rm --entrypoint d2 ghcr.io/esamson/atbp:latest --version
```

`package.sbt` aliases only `dockerPublish` (→ `cli / Docker / publish`), which
pushes to ghcr; use the scoped `publishLocal` task above for local checks.

## Project Structure

```
md2c/src/main/scala/ph/samson/atbp/md2c/
  D2.scala                  → NEW: render + transform for d2* fences
  DiagramFailure.scala       → NEW: soft-failure sibling insertion, shared
  Mermaid.scala              → edited: delegates sibling insertion to shared
  PlantUml.scala             → untouched
  StagedTree.scala           → edited: one line, D2.transform in the pipeline
md2c/src/test/scala/ph/samson/atbp/md2c/
  D2Spec.scala               → NEW, mirrors MermaidSpec
md2c/src/test/resources/
  markdown/d2/Shapes.md            → NEW parser fixture
  trees/D2 Diagram/{.md2c.conf,D2 Diagram.md}   → NEW staging fixture
  trees/D2 Failure/{.md2c.conf,D2 Failure.md}   → NEW soft-failure fixture
.cursor/rules/md2c.mdc       → edited: stack, conventions, boundaries
build.sbt                    → edited: cli dockerCommands installs d2
project/Dependencies.scala   → edited: D2Version, the single source of truth
```

**Not touched:** `github.sbt` and `.github/workflows/ci.yml`. See *CI* below —
this is deliberate, and it keeps `githubWorkflowCheck` out of the picture.

### Docker install step

d2 is not in apt for Ubuntu Noble, so the image fetches the pinned release
tarball and verifies it against the release's published `SHA256SUMS` before
installing. A failed checksum must fail the build.

- Version: `v0.9.0`, pinned as `Versions.D2` in `project/Dependencies.scala`
  so the string exists once and `dockerCommands` interpolates it.
- Asset: `d2-v0.9.0-linux-<arch>.tar.gz`, arch from `dpkg --print-architecture`
  (`amd64` / `arm64`) so the step stays correct if the image ever goes
  multi-arch; today `dockerPublish` builds single-arch on the CI runner.
- Added as a second `Cmd("RUN", …)` alongside the existing
  `git graphviz` apt step, cleaning up the tarball in the same layer.

With no runtime version check in md2c, **this pin is the compatibility
guarantee** for image users, which is why the Docker step ships with the
feature rather than after it.

### Pipeline placement

`D2.transform` runs after Mermaid, before Extensions:

```scala
plantUmlRendered <- PlantUml.transform(sourceDoc)
mermaidRendered  <- Mermaid.transform(plantUmlRendered)
d2Rendered       <- D2.transform(mermaidRendered)
extensionsRendered = Extensions.transform(d2Rendered)
```

### Shared soft-failure helper

`Mermaid.scala` currently holds ~50 lines of reflection-based sibling
insertion (`insertAfterParentIndex`, `insertFailureSiblings`) that D2 needs
verbatim. Rather than duplicate it, both move to `DiagramFailure.scala`
parameterized by the details header. This is the only edit to `Mermaid.scala`
and is behavior-preserving — **`MermaidSpec` passing unchanged is the guard**.
If that extraction turns out to be less contained than it looks, fall back to
duplicating in `D2.scala` and leave `Mermaid.scala` alone.

## Code Style

Follow `Mermaid.scala`. `D2.scala` shape:

```scala
object D2 {

  val RenderFailureComment = "# D2 rendering failed. See details below."
  val RenderFailureDetailsHeader = "D2 failure details:"
  val NotOnPathMessage = "Could not render diagram: d2 not on PATH"

  val DefaultFormat = "png"

  /** `(source, format)` so identical bodies with different fences do not
    * collide.
    */
  type RenderKey = (String, String)
  type RenderOutcome = Either[String, File]

  private def isD2(codeBlock: CodeBlock): Boolean =
    codeBlock.language().orElse("").startsWith("d2")

  private def formatFromLanguage(language: String): String =
    if (language.contains(".svg")) "svg"
    else if (language.contains(".png")) "png"
    else DefaultFormat
}
```

Conventions carried over: `ZIO.attemptBlocking` for process and file work,
`ZIO.collectAllPar` over the fence list, one `File.newTemporaryDirectory()`
per document, output named `fig-${index + 1}.d2.$format`, comment the
non-obvious (why exit code and not stderr; why no version check).

`render` takes the executable name as a parameter defaulting to `"d2"`. That
is the seam the missing-binary test uses — nothing else needs it, so it stays
`private[md2c]` rather than becoming a config option.

`RenderOutcome`'s `Left` is a plain `String` — Mermaid's is a
`JsExecutionResult` because mermaid-java hands one back; a subprocess only
gives us text.

### Invocation contract

```
d2 --stdout-format <png|svg> - -
```

- D2 source written to stdin, stream then closed.
- Success = **exit code 0 and non-empty stdout**; stdout bytes are the image.
  stderr is ignored on success (it carries a `success:` line).
- Failure = non-zero exit, or empty stdout. Details = captured stderr,
  trimmed; if stderr is empty, a fallback naming the exit code.
- A missing binary surfaces as `IOException` from `ProcessBuilder.start()`
  (`Cannot run program "d2": error=2, No such file or directory`), **not** as
  an exit code. That one exception type is caught and mapped to
  `NotOnPathMessage`; every other exception still fails the `Task`, matching
  Mermaid's "thrown exceptions are not caught" rule.
- `ZIO` timeout wraps the process so a wedged binary cannot hang a publish.

### Soft-failure presentation

Byte-for-byte parity with Mermaid, differing only in the comment marker (`#`
is D2's comment syntax, not `%%`) and the header text:

- Original block keeps its language (`d2`, `d2.png`, `d2.svg`); body becomes
  `RenderFailureComment` + `\n` + original source.
- Immediate sibling `CodeBlock`, language `text`, body
  `RenderFailureDetailsHeader` + `\n` + details.
- Details is d2's stderr for bad source (and for an unsupported flag on an old
  binary), or exactly `Could not render diagram: d2 not on PATH` when the
  binary is absent.

The missing-binary sibling therefore reads:

```
D2 failure details:
Could not render diagram: d2 not on PATH
```

**Decided:** the header is kept for every failure kind, including this one.
Uniform with Mermaid and with d2's own compile errors, one code path, no
special case — at the cost of slight redundancy, since the message already
reads as a complete sentence.

## Testing Strategy

`zio-test` (`ZIOSpecDefault`), `D2Spec.scala`, mirroring `MermaidSpec`'s
structure. Offline — the only external process is the local `d2` binary.

Tests are split by whether they need a real `d2` binary. This split is what
keeps most of the logic covered in CI, which will not have d2 installed.

**Group A — always runs, no binary required.** Driven through the
missing-binary path by passing a nonexistent executable name, so these cover
the whole failure-side ADF transformation without d2 present:

| Test | Asserts |
|---|---|
| binary absent → soft failure | AC 5; the exact `NotOnPathMessage` sibling |
| failure keeps the CodeBlock | AC 4 shape; original language preserved, `RenderFailureComment` prefix, source retained |
| failure sibling is a `text` CodeBlock | AC 4 shape; `RenderFailureDetailsHeader` present |
| failure nested in a `Panel` | sibling lands inside the panel, not at doc top level — exercises the reflection path in `DiagramFailure` |
| non-d2 CodeBlocks unchanged | AC 6 |
| `ParserSpec` parses a `d2/Shapes.md` fence | parsing, alongside the existing mermaid case |

**Group B — requires `d2` on `PATH`**, guarded by `.whenZIO(d2Available)`:

| Test | Asserts |
|---|---|
| default `d2` fence → PNG `MediaSingle` | AC 1; non-empty, `.d2.png` suffix, decoded `BufferedImage` larger than a degenerate 8×8 |
| `d2.png` fence → PNG | AC 2 |
| `d2.svg` fence → SVG | AC 2; file contains `<svg` |
| same source, `d2` + `d2.svg` | AC 3; two files, distinct formats |
| staging replaces d2, plantuml **and** mermaid fences on one page | AC 6; via `trees/D2 Diagram` |
| invalid d2 source → d2's own stderr as details | AC 4 details text; via `trees/D2 Failure` |

### The `d2Available` guard

`Spec.whenZIO` — the primitive zio-test's own `ifEnvSet` is built on — rather
than a custom aspect:

```scala
suite("d2 rendering")(/* Group B */).whenZIO(d2Available)
```

`d2Available: UIO[Boolean]` probes the binary once and never fails. Verified
in the zio-test 2.1.26 sources: on `false`, `whenZIO` annotates
`TestAnnotation.ignored` and yields `TestSuccess.ignored`, so **skipped tests
appear in the runner's ignored count** rather than disappearing. That visible
count is the only signal distinguishing "D2 verified" from "D2 not verified"
on a given run, so it must not be suppressed.

**Coverage expectation:** every acceptance criterion has a named test. AC 1–3
and the AC 4 *details text* are Group B and therefore verified locally only;
AC 4's *structure*, AC 5 and AC 6 are Group A and verified everywhere.
`PublisherSpec` stays `TestAspect.ignore`.

### CI

**Decided: CI does not install d2.** No `github.sbt` or `ci.yml` change, so
`githubWorkflowCheck` stays satisfied and CI keeps its current property of
having no external-binary dependencies (PlantUML and mermaid-java are both
pure JVM).

The accepted cost, stated plainly: **CI never renders a D2 diagram.** Group B
is ignored on every PR, so a regression in the subprocess invocation, the
image bytes, or the temp-file handling surfaces only in a local run or in
production. Group A keeps the failure path, the ADF shape and the parser
honest on every PR; the rendering itself is on whoever runs the suite locally.
Revisiting this means installing the pinned d2 via
`githubWorkflowBuildPreamble` and regenerating `ci.yml` in the same commit.

## Boundaries

**Always:**
- Keep unit tests offline; no Confluence, no network.
- Decide subprocess success by exit code, never by stderr being non-empty.
- Keep `(source, format)` as the render key so fence variants stay distinct.
- Catch only `IOException` from process start; let every other exception fail
  the `Task`.
- Run the full `AGENTS.md` commit loop: `git add -A` (including new files) →
  `sbt fixup` → `git status` → repeat until both clean, then one commit.
- Use `sbt --batch` for the run right after the `build.sbt` Docker edit;
  `sbt --client` otherwise.
- Treat `MermaidSpec` and `ParserSpec` passing unchanged as the regression gate
  on the shared-helper extraction.

**Ask first:**
- Adding any sbt dependency (none is planned).
- Any change to PlantUML or Mermaid *behavior* — the planned `Mermaid.scala`
  edit is a pure move of private helpers, nothing else.
- Changing the Docker base image, or the d2 version pinned in the image.
- Broadening the config surface (theme / layout / sketch / pad).
- Adding a d2 version check back in — decided against; reversing that is a
  new decision, not an implementation detail.
- Installing d2 in CI, or editing `github.sbt` / `ci.yml` — also decided
  against, for now.
- Un-ignoring `PublisherSpec`.

**Never:**
- Parse Markdown by hand — use `MarkdownParser` from adf-builder-java-markdown.
- Let a diagram failure of any kind fail the publish; diagram problems are
  always soft failures.
- Put HTTP in md2c, or reach for a network D2 renderer such as
  `play.d2lang.com`.
- Run `d2 --watch`, `d2 play`, or any interactive/long-running d2 subcommand.
- Auto-install or download the d2 binary at runtime on a user's machine.
- Suppress or hide the ignored-test count — it is the only signal that Group B
  did not run.
- Install the Docker d2 tarball without verifying its checksum.
- Commit secrets, or `git add` the `md2c/target/` build output.

## Success Criteria

- [ ] `sbt --client "md2c/testOnly *D2Spec*"` passes on this machine (d2
      present) with **zero ignored** — every Group A and Group B row exists
      and actually ran.
- [ ] With the `d2Available` probe forced false, the same run passes with
      Group B **ignored, not failed**, and Group A still green — this is the
      shape CI will see.
- [ ] `sbt --client "md2c/test"` passes with no edits to `MermaidSpec`
      assertions.
- [ ] `trees/D2 Diagram` stages to a doc whose `ExternalMedia` count matches
      its d2 + plantuml + mermaid fence count.
- [ ] `trees/D2 Failure` stages successfully — no `Task` failure — and yields
      a `d2` CodeBlock plus a `text` sibling.
- [ ] `docker run --rm --entrypoint d2 <image> --version` reports the pinned
      v0.9.0, so PNG needs no browser in the image.
- [ ] The image build fails if the d2 tarball checksum does not match
      `SHA256SUMS` (verify by perturbing the expected sum once).
- [ ] `Versions.D2` is the only place the version string appears.
- [ ] `.cursor/rules/md2c.mdc` documents the d2 fences, the `d2`-on-`PATH`
      requirement, the absence of a version check, and the soft-failure format.
- [ ] `sbt --client fixup && git status` leaves a clean tree in the same
      shell check, before the commit.

## Resolved Decisions

| # | Question | Decision |
|---|---|---|
| 1 | Renderer | Shell out to `d2` on `PATH`; no JVM D2 library exists |
| 2 | Missing / broken binary | Soft failure, Mermaid-style, message `Could not render diagram: d2 not on PATH` |
| 3 | Config surface | Fence suffixes only; d2 defaults for everything else |
| 4 | Distribution | Docker + docs ship with the feature |
| 5 | Version check | None. Docker pins the version; native users own their install |
| 6 | Failure details header | Kept for every failure kind, no special case |
| 7 | d2 install method | Pinned v0.9.0 tarball, verified against `SHA256SUMS` |
| 8 | CI | No d2 in CI; Group B tests skip via `whenZIO`, reported as ignored |

## Open Questions

None. Ready for `/plan`.

Two accepted risks carried forward, recorded here so they are not rediscovered
as surprises:

1. **CI does not verify D2 rendering** (decision 8). Group B is ignored on
   every PR; only the failure path and ADF shape are covered. Mitigation is
   the visible ignored count plus local runs.
2. **The Docker pin is the only compatibility guarantee** (decisions 5 and 7).
   A native user on a pre-v0.6.9 d2 gets an unsupported-flag soft failure; on
   v0.6.9–v0.8.x, PNG may attempt a Chromium download. Both surface as d2's
   own stderr on the published page rather than as a crash.
