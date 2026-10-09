# Tasks: GitHub Release zip

Spec: `SPEC.md`. Plan: `tasks/plan.md`.

## Task 1: Package the universal zip and upload it on `v*` tags

**Description:** After jars and the container publish, a `v*` tag builds `cli/Universal/packageBin` and uploads that zip as the `atbp` artifact. Pull requests and `main` do not upload it. `makeZip` stays the local command for the same archive.

**Acceptance criteria:**

- [ ] `githubWorkflowPublishPostamble` runs `cli/Universal/packageBin`, then uploads `cli/target/universal/*.zip` as artifact `atbp`
- [ ] Both steps are conditional on `startsWith(github.ref, 'refs/tags/v')`
- [ ] The upload fails the job when no zip is found
- [ ] `ci-release` and `dockerPublish` remain the publish steps, and the publish job still has `contents: read`
- [ ] `makeZip` in `package.sbt` is unchanged

**Verification:**

- [ ] `sbt --batch githubWorkflowGenerate` then `sbt --batch "githubWorkflowCheck"` both succeed
- [ ] Generated `publish` job shows the two steps after "Publish container", each with the tag `if`
- [ ] `sbt --batch fixup` leaves the working tree clean for the files in this task

**Dependencies:** None

**Files likely touched:**

- `github.sbt`
- `.github/workflows/ci.yml`

**Estimated scope:** Small: 1-2 files

## Task 2: Create the GitHub Release from that artifact

**Description:** A `release` job downloads `atbp` and creates the GitHub Release for the tag. Final `vMAJOR.MINOR.PATCH` tags are normal releases. Every other `v*` tag is a pre-release. Notes are auto-generated on first create. A re-run replaces the zip.

**Acceptance criteria:**

- [ ] Job id is `release`, it `needs: [publish]`, and its `if` is `startsWith(github.ref, 'refs/tags/v')`
- [ ] The job's only added permission is `contents: write`
- [ ] Its matrix is a single row: `ubuntu-latest`, the project Scala version, `temurin@25`
- [ ] It downloads artifact `atbp` into `dist` and runs the `SPEC.md` `gh` script
- [ ] The script passes `--prerelease` only when the tag does not match `^v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$`
- [ ] First create uses `--generate-notes` and `--title` set to the tag. An existing release is updated with `gh release upload --clobber` and its notes stay as they are
- [ ] The script fails when `dist` does not contain exactly one `*.zip`

**Verification:**

- [ ] `sbt --batch githubWorkflowGenerate` then `sbt --batch "githubWorkflowCheck"` both succeed
- [ ] Generated `release` job matches the acceptance criteria, and the publish job's permissions are unchanged
- [ ] Classifier check, using the same regular expression as the generated script:

```bash
classify() {
  local tag="$1"
  if [[ "$tag" =~ ^v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$ ]]; then
    echo release
  else
    echo prerelease
  fi
}
test "$(classify v1.2.3)" = release
test "$(classify v0.1.0)" = release
test "$(classify v1.2.3-RC1)" = prerelease
test "$(classify v1.2.3-M1)" = prerelease
test "$(classify v1.2.3-alpha.1)" = prerelease
test "$(classify v1.2)" = prerelease
test "$(classify v1)" = prerelease
```

- [ ] `sbt --batch fixup` leaves the working tree clean for the files in this task

**Dependencies:** Task 1

**Files likely touched:**

- `github.sbt`
- `.github/workflows/ci.yml`

**Estimated scope:** Small: 1-2 files

## Checkpoint: After Tasks 1-2

- [ ] `sbt --batch "githubWorkflowCheck"` passes
- [ ] Generated workflow matches `SPEC.md`: tag-only zip upload, `release` needs `publish`, `contents: write` only on `release`, auto-generated notes, pre-release rule, re-run replaces the zip
- [ ] Classifier check above passes
- [ ] Review with a human before proceeding

## Task 3: Document the GitHub Release zip in `README.md`

**Description:** Add a GitHub release install section in the same voice as "Local installation". A reader can download the zip, see that Java 25 is required, and run `bin/atbp`.

**Acceptance criteria:**

- [ ] `README.md` tells the reader to download `atbp-<version>.zip` from GitHub Releases
- [ ] It states that the unpacked distribution requires Java 25
- [ ] It states how to run `bin/atbp`
- [ ] The existing local-install section stays

**Verification:**

- [ ] Manual check: read `README.md` against the acceptance criteria
- [ ] No workflow or sbt files change in this task

**Dependencies:** None. May proceed alongside Tasks 1-2. The complete checkpoint waits for this task as well.

**Files likely touched:**

- `README.md`

**Estimated scope:** Small: 1-2 files

## Checkpoint: Complete

- [ ] `SPEC.md` success criteria are met by the generated workflow and `README.md`
- [ ] `sbt --batch "githubWorkflowCheck"` passes
- [ ] A live tag push remains out of band
- [ ] Ready for review
