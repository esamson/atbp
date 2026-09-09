# Todo: D2 diagram support in md2c

Plan: [tasks/plan.md](plan.md) · Spec: [SPEC.md](../SPEC.md)

Legend: `[ ]` pending · `[~]` in progress · `[x]` done
Sizes: XS (1 file) · S (1-2) · M (3-5)

## Phase 1 — Foundation

- [x] **Task 1 — Extract `DiagramFailure.scala`** (S · deps: none)
  - [x] `DiagramFailure.insertFailureSiblings(adf, errorByBlock, header)` +
        `insertAfterParentIndex` moved out of `Mermaid.scala`
  - [x] `Mermaid` keeps its two public failure-string constants
  - [x] Zero edits to `MermaidSpec` assertions (`git diff` empty)
  - [x] `sbt --client "md2c/testOnly *MermaidSpec*"` green ← gate
  - [x] `sbt --client "md2c/test"` green
  - [ ] Fallback if the reflection path resists: revert, duplicate privately
        in `D2.scala`, note it in the commit
  - [x] Commit via the `AGENTS.md` loop (own commit — do not fold into Task 2)

- [x] **Checkpoint: Foundation** — `md2c/test` green, `MermaidSpec` /
      `ParserSpec` untouched, tree clean

## Phase 2 — Core vertical slice

- [x] **Task 2 — `D2.scala` + pipeline wiring** (M · deps: T1)
  - [x] `render(adf, executable = "d2")`, `private[md2c]` executable seam
  - [x] `d2 --stdout-format <png|svg> - -`; stdin written then closed
  - [x] `redirectOutput` → `fig-${index+1}.d2.$format`, `redirectError` → temp
        file (no pipes — deadlock hazard above 64KB)
  - [x] Success = exit 0 **and** non-empty output file; stderr ignored
  - [x] Failure details = trimmed stderr, else `d2 exited with code $n`
  - [x] `Process.waitFor(30, SECONDS)` + `destroyForcibly()`; timeout →
        soft failure `Could not render diagram: d2 timed out after 30s`
  - [x] Catch only `IOException` from process start → `NotOnPathMessage`
  - [x] `(source, format)` render key; one temp dir per doc;
        `ZIO.collectAllPar`
  - [x] Comments: why exit code not stderr; why no version check
  - [x] `StagedTree.convert`: `D2.transform` after Mermaid, before Extensions
  - [x] `d2Available: UIO[Boolean]` + `.whenZIO(d2Available)` on Group B suite
  - [x] Test: default `d2` fence → `.d2.png` `ExternalMedia`, PNG decodes
        `> 8×8` (AC 1)
  - [x] Test: nonexistent executable → `NotOnPathMessage` sibling (AC 5)
  - [x] `sbt --client "md2c/testOnly *D2Spec*"` — zero ignored
  - [x] `sbt --client "md2c/test"` green
  - [x] Probe the timeout path once with a sleeping stub script; confirm soft
        failure, not a hang; revert the probe

- [x] **Task 3 — Group A: rest of the failure path + parser** (S · deps: T2)
  - [x] Failure keeps `CodeBlock`, original language preserved for `d2`,
        `d2.png`, `d2.svg`; body = `RenderFailureComment` + source (AC 4)
  - [x] Sibling is a `text` `CodeBlock` under `RenderFailureDetailsHeader`
  - [x] Nested `Panel` case: sibling lands inside the panel
        (`panel` code-block count == 2)
  - [x] Non-d2 blocks (`scala`, `plantuml`, `mermaid`) unchanged, no
        `ExternalMedia` (AC 6)
  - [x] `markdown/d2/Shapes.md` fixture + `ParserSpec` case
        (`doc.isSupported`)
  - [x] `sbt --client "md2c/testOnly *D2Spec*"` green
  - [x] `sbt --client "md2c/testOnly *ParserSpec*"` green

- [x] **Task 4 — Group B: format variants** (XS-S · deps: T2)
  - [x] `d2.png` → non-empty `.d2.png` (AC 2)
  - [x] `d2.svg` → non-empty `.d2.svg` containing `<svg` (AC 2)
  - [x] Same source as `d2` + `d2.svg` → two files, distinct formats (AC 3)
  - [x] `sbt --client "md2c/testOnly *D2Spec*"` — zero ignored
  - [x] Force `d2Available` false: Group B **ignored not failed**, Group A
        green, exit 0; then revert the forcing

- [ ] **Checkpoint: Core slice** — `md2c/test` green; `D2Spec` zero ignored
      locally; forced-false run correct; AC 1-6 each have a named test;
      `MermaidSpec` assertions untouched; **human review before Phase 3**

## Phase 3 — Staging fixtures

- [ ] **Task 5 — End-to-end staging trees** (M · deps: T2)
  - [ ] `trees/D2 Diagram/{.md2c.conf, D2 Diagram.md}` — d2 + plantuml +
        mermaid on one page (`spaceId = 1234567`)
  - [ ] Test: no `d2*`/`plantuml*`/`mermaid*` languages left;
        `ExternalMedia` count == fence count (AC 6)
  - [ ] `trees/D2 Failure/{.md2c.conf, D2 Failure.md}` — invalid d2 source
  - [ ] Test: stages with no `Task` failure; `d2` `CodeBlock` + `text` sibling
        carrying d2's own stderr; no `ExternalMedia` (AC 4 details text)
  - [ ] `sbt --client "md2c/testOnly *StagedTreeSpec*"` still green
  - [ ] `sbt --client "md2c/test"` green

## Phase 4 — Distribution

- [ ] **Task 6 — Pinned `d2` v0.9.0 in the container image** (S · deps: none)
  - Engine is **podman** throughout — no `docker` binary exists on this
    machine. sbt's `Docker / …` task names are the plugin's spelling, keep them
  - [ ] **Prereq:** `podman machine start` (`podman-machine-default` is down)
  - [ ] **Prereq:** `dockerExecCommand := Seq("podman")` in `local.sbt`
        (gitignored, never committed) — the plugin defaults to a `docker`
        binary that is not here. Fallback: `Docker / stage` then
        `podman build cli/target/docker/stage/`
  - [ ] **Prereq:** `podman run --rm eclipse-temurin:25-jre-noble sh -c
        'command -v curl wget'` — if absent, add `curl ca-certificates` to the
        existing `git graphviz` apt step, not a third layer
  - [ ] **Prereq:** `tar tzf` the asset to locate the binary inside it
  - [ ] `Versions.D2 = "v0.9.0"` in `project/Dependencies.scala`
  - [ ] Second `Cmd("RUN", …)`: fetch `d2-v0.9.0-linux-$(dpkg
        --print-architecture).tar.gz` + `SHA256SUMS`, verify with
        `sha256sum -c`, install, delete the tarball in the same layer
  - [ ] `grep -rn "0\.9\.0" build.sbt project/` → only `Versions.D2`
  - [ ] `sbt --batch "cli / Docker / publishLocal"` succeeds (`--batch`: build
        files edited in this task)
  - [ ] `podman run --rm --entrypoint d2 ghcr.io/esamson/atbp:latest --version`
        reports `v0.9.0` (AC 7)
  - [ ] Render a d2 fence inside the container — no host setup needed
  - [ ] Perturb the expected sum once → build **fails**; restore, rebuild clean
  - [ ] `sbt --batch githubWorkflowCheck` still passes
  - [ ] `github.sbt` and `.github/workflows/ci.yml` untouched

- [ ] **Checkpoint: Distribution** — image renders d2 with no host setup;
      checksum failure demonstrated and reverted; `githubWorkflowCheck` green

## Phase 5 — Documentation

- [ ] **Task 7 — `.cursor/rules/md2c.mdc`** (XS · deps: T1-T6)
  - [ ] Stack: D2 via the `d2` CLI, alongside PlantUML and Mermaid
  - [ ] Conventions: `d2` / `d2.png` / `d2.svg` fences; success →
        `MediaSingle`; soft-failure strings verbatim
  - [ ] `d2` must be on `PATH`; no version check; the image pins the version
  - [ ] Commands: `sbt --client "md2c/testOnly *D2Spec*"`
  - [ ] Note that Group B skips without `d2` and reports as ignored
  - [ ] Cross-check every documented string against a constant in `D2.scala`

- [ ] **Checkpoint: Complete** — spec AC 1-7 met; every box in the spec's
      *Success Criteria* ticked; `sbt --client fixup && git status` clean in
      one check; ready for review

## Decisions (settled — do not reopen)

- One commit per task, seven total; Task 1 always its own commit
- Repo keeps docker for GitHub publishing — `github.sbt` / `ci.yml` untouched;
  podman override lives only in gitignored `local.sbt`
- `tasks/` is committed to version control
