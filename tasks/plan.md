# Implementation Plan: GitHub Release zip

## Overview

On a `v*` tag, the existing publish job builds the universal zip from `cli / Universal / packageBin` and uploads it as an artifact. A new `release` job then creates the GitHub Release, attaches that zip, and writes auto-generated notes. Final tags (`vMAJOR.MINOR.PATCH`) are normal releases. Every other `v*` tag is a pre-release. `README.md` documents how to download and run the zip. This follows `SPEC.md`.

## Architecture Decisions

- Extend the generated workflow in `github.sbt`. `githubWorkflowPublishPostamble` holds the tag-only package and upload steps, which run after `ci-release` and `dockerPublish`. `githubWorkflowAddedJobs` adds the `release` job. The existing `githubWorkflowGeneratedCI` mapping already reads the plugin definition, which appends added jobs, and leaves any job other than `publish` unchanged.
- The `release` job `needs: [publish]` and is limited to `startsWith(github.ref, 'refs/tags/v')`. Its only permission is `contents: write`. The publish job keeps `contents: read`.
- `sbt-github-actions` 0.32.1 always renders an `os` / `scala` / `java` matrix for every job, and `WorkflowJob` defaults to Scala 2.13.10 and Zulu 8. The release job sets `oses = List("ubuntu-latest")`, `scalas = List(scalaVersion.value)`, and `javas = List(JavaSpec.temurin("25"))` so the matrix is a single row matching publish. The job's own steps do not set up Java or sbt. They download the artifact and run `gh`.
- Artifact name is `atbp`. Upload path is `cli/target/universal/*.zip` with `if-no-files-found: error`. Download path is `dist`. The GitHub Release asset keeps the packager filename `atbp-<version>.zip`. Action pins match the workflow already generated for this repo: `actions/upload-artifact@v7` and `actions/download-artifact@v8`.
- The shell in the release job is the script in `SPEC.md`. It treats a tag as final when it matches `^v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$`. A re-run that finds an existing release uploads the zip with `--clobber` and leaves the existing notes alone. The script exits non-zero unless `dist` contains exactly one `*.zip`.
- `makeZip` stays `show cli / Universal / packageBin`. CI runs the underlying task.
- Regenerated `.github/workflows/ci.yml` is committed with the `github.sbt` change. It is not hand-edited.
- A live tag push is out of band. Verification is `githubWorkflowCheck`, inspection of the generated workflow, and a local run of the tag classifier.

## Task List

Full acceptance criteria, verification, and file lists are in `tasks/todo.md`.

### Phase 1: Release pipeline

- [ ] Task 1: Package the universal zip and upload it on `v*` tags
- [ ] Task 2: Create the GitHub Release from that artifact

### Checkpoint: Release pipeline

- [ ] `githubWorkflowCheck` passes
- [ ] Generated `publish` and `release` jobs match `SPEC.md`
- [ ] Tag classifier matches the spec table
- [ ] Review with a human before proceeding

### Phase 2: Install docs

- [ ] Task 3: Document the GitHub Release zip in `README.md`

### Checkpoint: Complete

- [ ] `SPEC.md` success criteria are met
- [ ] `githubWorkflowCheck` still passes
- [ ] Ready for review

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| `WorkflowJob` defaults emit a 2.13.10 / Zulu 8 matrix | Medium | Set `oses`, `scalas`, and `javas` explicitly on the release job and confirm the generated matrix |
| Added job dropped by the `githubWorkflowGeneratedCI` override | High | Generate and confirm a `release` job that `needs: [publish]` is present in `ci.yml` |
| Multiline shell rewritten by the workflow renderer | High | Read the generated `run:` block and confirm the regex, `--generate-notes`, `--prerelease`, and `--clobber` survived |
| Postamble steps also run on `main` | High | Set `cond` to `startsWith(github.ref, 'refs/tags/v')` on the package and upload steps |
| `contents: write` applied to the publish job | Medium | Set that permission only on the release job and confirm the publish job still has `contents: read` |
| Zero or several zips uploaded | Medium | `if-no-files-found: error` on upload, and the release script requires exactly one `dist/*.zip` |
| Release behavior unverified against GitHub | Low | Live tag creation stays out of band; review the generated `gh` invocation against `SPEC.md` |

## Parallelization Opportunities

Tasks 1 and 2 share `github.sbt` and the generated workflow, so they stay sequential. Task 3 only touches `README.md` and can proceed alongside Phase 1. The complete checkpoint still waits for all three.

## Open Questions

None. `SPEC.md` has no open questions, and the plugin extension points above are fixed by `sbt-github-actions` 0.32.1.
