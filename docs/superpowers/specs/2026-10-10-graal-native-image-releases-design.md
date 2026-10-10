# Graal native-image releases

## Goal

Publish GraalVM native images of the `atbp` CLI for Linux x86_64, Linux aarch64, and macOS Apple Silicon on GitHub Releases, next to the existing Java 25 universal zip.

The binary is the full CLI, the same main class and the same commands as the zip. CI does not call Jira or Confluence. Those commands are present and answer `--help`. They are not invoked.

## Non-goals

- Replacing or removing the universal zip.
- Static or musl builds. Linux binaries are dynamically linked against glibc.
- macOS code signing or notarization.
- Building native images on pull requests.
- Moving to a larger GitHub-hosted runner. If a hosted runner runs out of memory, that is a later change.
- Bundling `d2` or Graphviz inside the binary. They stay programs on `PATH`.
- Changing `sbt installLocal`. That remains a JVM install.
- Publishing the native binaries to Maven or GHCR.

## Pipeline

CI stays generated from `github.sbt`. Do not hand-edit `.github/workflows/ci.yml`. Regenerate it with `githubWorkflowGenerate`.

`build` and `publish` are unchanged. `build` still tests on `ubuntu-latest` with Temurin 25 and uploads its `target` tarball. `publish` still publishes jars, the container, and, on `v*` tags, the universal zip.

Add a `native` job. It runs only when the event is not a pull request and the ref is `main` or a `v*` tag. It needs `build`. Its matrix fails fast. Each cell has a 90-minute timeout.

| Runner | Release asset |
|---|---|
| `ubuntu-24.04` | `atbp-linux-amd64` |
| `ubuntu-24.04-arm` | `atbp-linux-arm64` |
| `macos-latest` | `atbp-darwin-arm64` |

`macos-latest` is the Apple Silicon hosted runner.

Added jobs do not inherit the default checkout and setup steps, so this job lists them itself:

1. Checkout.
2. Install GraalVM Community for JDK 25 via `JavaSpec.graalvm(Graalvm.Distribution("graalvm-community"), "25")`, which emits `graalvm/setup-graalvm` with the `native-image` component.
3. Setup sbt.
4. Download and inflate the `build` job's target artifact. That artifact is named `target-ubuntu-latest-3.9.0-temurin@25` under the current build matrix. Key the download off the build job, not off the native cell's OS or Java. Class files are platform-independent. A zinc rebuild is acceptable. Tests are not re-run.
5. Run `cli/GraalVMNativeImage/packageBin`.
6. Install Graphviz and the pinned `d2`, then run the smoke script.
7. On a `v*` tag only, upload the renamed binary as an Actions artifact whose name is the release asset name. On `main`, a green smoke test is the end of the job. Do not upload artifacts on `main`.

The workflow-wide `SBT_OPTS=-Xmx12G` is overridden on this job with `SBT_OPTS=-Xmx2G`, so sbt's heap is not stacked on the native-image heap.

`release` needs both `publish` and `native`. It downloads the zip artifact into `dist/` and the three native artifacts into `native/`, then attaches all four files to the GitHub Release. A re-run against an existing Release uploads with `--clobber`, as the zip does today. The script requires exactly one file in `dist/` and exactly `atbp-linux-amd64`, `atbp-linux-arm64`, and `atbp-darwin-arm64` in `native/`. Any other count is an error.

## Image build

`cli` already enables `JavaAppPackaging`. Also enable `GraalVMNativeImagePlugin`.

Leave `graalVMNativeImageGraalVersion` unset. The plugin then calls the `native-image` on `PATH` and writes `cli/target/graalvm-native-image/atbp`. Set the image name to `atbp`. The plugin otherwise uses the project name `atbp-cli`. CI renames `atbp` to the platform asset name only at upload time.

The build is the default dynamic link: glibc on Linux, the normal dynamic link on macOS. Do not pass `--static` or `--libc=musl`. Do not build inside the plugin's Graal Docker image.

`graalVMNativeImageOptions` includes:

- `--no-fallback`, so a failed image is a failed build rather than a binary that still needs a JVM.
- `-J-Xmx6g`, the native-image heap cap.

Add further flags only when the build or the smoke suite requires them.

Reachability metadata is checked in at `cli/src/main/resources/META-INF/native-image/`. Produce it locally with the tracing agent by running the JVM-packaged CLI through the same smoke commands, then commit the agent's `reachability-metadata.json`. CI does not run the agent.

GraalJS, which Mermaid uses, is the likely hard part. The image is not done until the Mermaid fence in the smoke fixture renders. If that cannot be made to work, stop and revise this design. Do not ship a binary that drops diagrams.

## Version

A native image does not expose the JAR manifest through `Package.getImplementationVersion`. The image build passes the sbt `version` setting as the system property `ph.samson.atbp.version`.

`atbp --version` prints, in order:

1. `v` plus `ph.samson.atbp.version`, when that property is non-empty.
2. `v` plus the JAR implementation version, when the property is absent. This is the universal zip.
3. `(dev)`, when neither is set.

sbt's `version` has no leading `v`. The job sets the expected string to `v` plus the last line of `sbt --client "print version"`, and passes that string to the smoke script.

## Smoke gate

`cli/native-smoke/smoke.sh` takes the binary path and the expected version string. The `native` job runs it after the image build and before any upload. A non-zero exit fails the cell.

The script sets `XDG_CONFIG_HOME` to a temporary directory containing `atbp/application.conf` with exactly:

```hocon
atbp {
}
```

It does not read the runner's real config, and it needs no Jira or Confluence secrets.

Checks, in order:

1. `atbp --version` equals the expected version and is not `(dev)`.
2. `--help` exits 0 for `atbp` and for every command: `hubad`, `liga`, `liga leaderboard`, `liga handicap`, `liga serve`, `md2c`, `mdmerge`, `plate`, `plate label`, `plate check`, `plate radar`, `plate sort`, `retext`, `stmt2csv`, `traceviz`.
3. `atbp retext --target <dir> cli/native-smoke/fixtures/note.md` writes `<dir>/note.md`, and that file contains the fixture's text.
4. `atbp stmt2csv --text --target <file> cli/native-smoke/fixtures/hello.pdf` writes `<file>`, and that file contains `native-smoke`.
5. `atbp md2c --render-only --target <dir> cli/native-smoke/fixtures/diagrams` renders `diagrams.md` in that directory. `<dir>` contains non-empty `plantuml.png`, `mermaid.png`, and `d2.png`. The command exits 0. Standard output and standard error do not contain `Mermaid rendering failed`, `D2 rendering failed`, or `d2 not on PATH`.

Before the script, the job installs Graphviz (`apt-get install graphviz` on Ubuntu, `brew install graphviz` on macOS) and `d2` v0.9.0 (`Dependencies.Versions.D2`). The `d2` install uses the same release URL and `SHA256SUMS` check as the container image (`https://github.com/terrastruct/d2/releases/download/v0.9.0`). Archive names:

| Runner | Archive |
|---|---|
| `ubuntu-24.04` | `d2-v0.9.0-linux-amd64.tar.gz` |
| `ubuntu-24.04-arm` | `d2-v0.9.0-linux-arm64.tar.gz` |
| `macos-latest` | `d2-v0.9.0-macos-arm64.tar.gz` |

Extract `d2-v0.9.0/bin/d2` onto `PATH`, the same path the container image extracts. A missing `d2` or `dot` binary fails the job before the smoke script runs.

## `md2c --render-only`

This is the only new CLI behavior. Today PlantUML, Mermaid, and D2 run only inside a Confluence publish, so the native binary would otherwise have no offline way to render a diagram.

`atbp md2c --render-only --target <dir> <sourceDir>` parses each non-dot `*.md` file directly in `sourceDir` (not recursive) with the same markdown parser md2c already uses, runs the existing PlantUML, Mermaid, and D2 transforms, and writes:

- `plantuml.png` for a `plantuml` fence
- `mermaid.png` for a `mermaid` fence
- `d2.png` for a `d2.png` fence

`cli/native-smoke/fixtures/diagrams/diagrams.md` contains one `plantuml` fence, one `mermaid` fence, and one `d2.png` fence. A second fence of the same kind is an error, as is a directory with no markdown files. A fence that does not produce its image makes `--render-only` exit non-zero. Publish behavior is unchanged: a bad diagram there is still a soft failure.

The command does not load `SourceTree` page identity, does not reserve page IDs, and does not build a Confluence or HTTP client. It does not need `spaceKey`, `pageId`, or a Confluence section in config.

`--render-only` and `--cleanup` together are an error. `md2c` without `--render-only` still publishes and still requires Confluence config.

## Release assets and README

A `v*` GitHub Release attaches these uncompressed executables beside the existing zip:

- `atbp-linux-amd64`
- `atbp-linux-arm64`
- `atbp-darwin-arm64`

The README packaged-releases section lists all four downloads.

- The zip still requires Java 25.
- The Linux binaries are built on Ubuntu 24.04 and need glibc 2.39 or newer. They do not run on Alpine.
- The macOS binary is an unsigned Apple Silicon executable. The README says to `chmod +x` it and to run `xattr -d com.apple.quarantine` on it before the first run. `chmod +x` is also documented for the Linux binaries, because a browser download does not keep the executable bit.
- `d2` and Graphviz remain separate installs. The image does not include them.

## Failures

`native-image` or the smoke script failing on any runner fails that cell. Because the matrix fails fast and `release` needs `native`, the tag gets no GitHub Release, zip included. Jars and the container may already be published from `publish`. That split already exists between `publish` and `release`.

A missing release asset is an error. The smoke script's failure text, a `(dev)` version, and a missing image file are hard failures. The production `md2c` publish path still soft-fails a single bad diagram. The smoke script does not.

Running out of memory on the hosted runner fails the job. Raising the heap past `-J-Xmx6g` or switching to a larger runner is outside this design.

## Testing

- A JVM test covers `--render-only`: the three images are written, the failure strings are absent, and no Confluence config is required. Passing `--render-only` and `--cleanup` together fails.
- The native smoke script is the release gate for the built binary. It is what runs on `main` and on tags.
- Existing JVM test suites stay the coverage for command behavior. They are not re-run inside the native image.
