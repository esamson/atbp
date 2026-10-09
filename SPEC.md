# Spec: GitHub Release zip

## Objective

People who want the `atbp` CLI without building from source can download a packaged zip from the GitHub Releases page. Continuous integration creates that release when a `v*` tag is pushed, attaches the universal zip that `makeZip` builds, and writes GitHub's auto-generated notes.

A final tag is published as a normal release. Any other `v*` tag is published as a pre-release.

Success for a tag `v1.2.3` is a published GitHub Release titled `v1.2.3`, with auto-generated notes and a single asset `atbp-1.2.3.zip`. Success for a tag `v1.2.3-RC1` is the same, marked as a pre-release, with asset `atbp-1.2.3-RC1.zip`.

## Tech Stack

- sbt  (existing build), Scala 3.9.0, Java Temurin 25
- `sbt-github-actions` 0.32.1 generates `.github/workflows/ci.yml` from `github.sbt`
- `sbt-native-packager` 1.12.0 `JavaAppPackaging` produces the zip via `cli / Universal / packageBin`
- `sbt-dynver` supplies the version baked into the zip name. On tag `v1.2.3` the version is `1.2.3`
- `gh` on the GitHub-hosted `ubuntu-latest` runner creates the release with the workflow `GITHUB_TOKEN`
- Existing publish steps stay: `ci-release` (Sonatype) and `dockerPublish` (GHCR)

The zip is a Java application distribution (`bin/atbp` plus `lib/`). Running it requires Java 25 on the machine that unpacks it.

## Commands

```bash
# Local zip. Prints the path of cli/target/universal/atbp-<version>.zip
sbt --client makeZip

# After editing github.sbt or any other *.sbt / project/* file
sbt --batch githubWorkflowGenerate
sbt --batch "githubWorkflowCheck"

# Routine check when build definition files are unchanged
sbt --client "githubWorkflowCheck"

# Format and lint the whole build before commit
sbt --client fixup
```

`makeZip` remains `show cli / Universal / packageBin`. The release pipeline runs the underlying task `cli/Universal/packageBin` and uploads the zip that task writes.

## Project Structure

```
github.sbt                         → workflow description: package step, artifact, release job
.github/workflows/ci.yml           → generated; committed; regenerated, never hand-edited
README.md                          → "GitHub release" install section
package.sbt                        → makeZip alias stays the local entry point
cli/target/universal/atbp-*.zip    → build output; not committed
```

## Code Style

Workflow changes are Scala in `github.sbt`, matching the existing `WorkflowStep` declarations. The publish job gains two tag-only steps after `ci-release` and `dockerPublish`. A separate job creates the release so `contents: write` applies only to that job.

```scala
WorkflowStep.Sbt(
  commands = List("cli/Universal/packageBin"),
  name = Some("Package universal zip"),
  cond = Some("startsWith(github.ref, 'refs/tags/v')")
)
```

The release job depends on `publish`, runs only for `refs/tags/v*`, and has `contents: write`. It downloads the uploaded zip and runs `gh`. A tag is final when it matches `^v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$`. Every other `v*` tag is a pre-release.

```bash
TAG="${GITHUB_REF_NAME}"
ZIP=$(echo dist/*.zip)
if [[ "$TAG" =~ ^v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$ ]]; then
  PRE=()
else
  PRE=(--prerelease)
fi

if gh release view "$TAG" >/dev/null 2>&1; then
  gh release upload "$TAG" "$ZIP" --clobber
else
  gh release create "$TAG" "$ZIP" \
    --title "$TAG" \
    --generate-notes \
    "${PRE[@]}"
fi
```

A re-run that finds an existing release replaces the zip and leaves the existing notes in place. The first successful create is what writes the notes and the pre-release flag.

README prose matches the current install section: short, imperative, with the exact commands a person runs.

## Testing Strategy

This behavior is a generated workflow plus a shell decision. There is no new Scala test suite.

- `sbt githubWorkflowCheck` is the CI gate. The committed `.github/workflows/ci.yml` must match generation from `github.sbt`.
- The generated publish job, on a `v*` tag, runs `cli/Universal/packageBin` and uploads exactly that zip as an artifact named `atbp-universal`, failing the job when the zip is missing.
- The generated `release` job needs `publish`, is limited to `refs/tags/v*`, sets `contents: write`, downloads that artifact, and runs the `gh` script above.
- Classifier examples, checked against the script's regular expression:

| Tag | GitHub Release |
|---|---|
| `v1.2.3` | Release |
| `v0.1.0` | Release |
| `v1.2.3-RC1` | Pre-release |
| `v1.2.3-M1` | Pre-release |
| `v1.2.3-alpha.1` | Pre-release |
| `v1.2` | Pre-release |
| `v1` | Pre-release |

- Pull requests and branch `main` do not run the release job. Sonatype and GHCR publishing on `main` and on `v*` tags stay as they are.
- Cutting a real tag is out of band for this change. Review of the generated workflow is the verification that a release would be created.

## Boundaries

- Always: describe workflow changes in `github.sbt` and regenerate `ci.yml` with `githubWorkflowGenerate`. Keep `makeZip` as the local command for the same zip. Create the GitHub Release only from the tag-gated release job, after `publish` succeeds. Use auto-generated notes. Mark non-final `v*` tags as pre-releases. Document the zip install in `README.md`, including the Java 25 requirement. Run `fixup` before committing Scala or sbt changes.
- Ask first: changing the `v*` tag pattern, attaching any asset besides the universal zip, adding a checksum file, pinning a new GitHub Action, marking final releases as drafts, or giving `contents: write` to the existing publish job.
- Never: hand-edit `.github/workflows/ci.yml`. Create a GitHub Release from a pull request or from `main`. Publish the zip to Maven. Commit the zip or any secret. Drop `ci-release` or `dockerPublish`.

## Success Criteria

- Pushing tag `v1.2.3` produces a published GitHub Release titled `v1.2.3` with auto-generated notes and asset `atbp-1.2.3.zip`.
- Pushing tag `v1.2.3-RC1` produces a pre-release titled `v1.2.3-RC1` with auto-generated notes and asset `atbp-1.2.3-RC1.zip`.
- The asset is the archive written by `cli / Universal / packageBin`.
- A re-run of the workflow for the same tag replaces that asset.
- The release job does not run for pull requests or for `main`.
- `README.md` tells a reader how to download `atbp-<version>.zip` from GitHub Releases, that they need Java 25, and how to run `bin/atbp`.
- `githubWorkflowCheck` passes on the committed workflow.

## Open Questions

None. Final versus pre-release is the regular expression in Code Style. A re-run replaces the zip and keeps notes already on the release.
