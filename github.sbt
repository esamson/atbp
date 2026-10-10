import scala.concurrent.duration._

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("25"))

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.StartsWith(Ref.Tag("v")),
  RefPredicate.Equals(Ref.Branch("main"))
)

ThisBuild / githubWorkflowEnv += "SBT_OPTS" -> "-Xmx12G"

ThisBuild / githubWorkflowGeneratedCI := (ThisBuild / githubWorkflowGeneratedCI).value
  .map {
    case publish if publish.id == "publish" && publish.permissions.isEmpty =>
      publish.copy(
        permissions = Some(
          Permissions.Specify(
            Map(
              PermissionScope.Contents -> PermissionValue.Read,
              PermissionScope.Packages -> PermissionValue.Write,
              PermissionScope.IdToken -> PermissionValue.Write
            )
          )
        )
      )
    case other => other
  }

ThisBuild / githubWorkflowPublishPreamble := Seq(
  WorkflowStep.Use(
    UseRef.Public("docker", "setup-qemu-action", "v4"),
    name = Some("Set up QEMU")
  ),
  WorkflowStep.Use(
    UseRef.Public("docker", "setup-buildx-action", "v4"),
    name = Some("Set up Docker Buildx")
  ),
  WorkflowStep.Run(
    commands = List(
      """echo "${{ secrets.GITHUB_TOKEN }}" | docker login ghcr.io -u ${{ github.actor }} --password-stdin"""
    ),
    name = Some("Log in to registry")
  )
)

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    commands = List("ci-release"),
    name = Some("Publish jars"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  ),
  WorkflowStep.Sbt(
    commands = List("dockerPublish"),
    name = Some("Publish container")
  )
)

ThisBuild / githubWorkflowPublishPostamble := Seq(
  WorkflowStep.Sbt(
    commands = List("cli/Universal/packageBin"),
    name = Some("Package universal zip"),
    cond = Some("startsWith(github.ref, 'refs/tags/v')")
  ),
  WorkflowStep.Use(
    UseRef.Public("actions", "upload-artifact", "v7"),
    name = Some("Upload universal zip"),
    cond = Some("startsWith(github.ref, 'refs/tags/v')"),
    params = Map(
      "name" -> "atbp",
      "path" -> "cli/target/universal/*.zip",
      "if-no-files-found" -> "error"
    )
  )
)

val releaseGhScript =
  """set -euo pipefail
shopt -s nullglob
zips=(dist/*.zip)
if (( ${#zips[@]} != 1 )); then
  echo "expected exactly one dist/*.zip, found ${#zips[@]}" >&2
  exit 1
fi
required=(atbp-linux-amd64 atbp-linux-arm64 atbp-darwin-arm64)
native_files=(native/*)
if (( ${#native_files[@]} != 3 )); then
  echo "expected exactly 3 files in native/, found ${#native_files[@]}" >&2
  exit 1
fi
for name in "${required[@]}"; do
  if [[ ! -f "native/$name" ]]; then
    echo "missing native/$name" >&2
    exit 1
  fi
done
TAG="${GITHUB_REF_NAME}"
ZIP=$(echo dist/*.zip)
assets=("$ZIP" "native/atbp-linux-amd64" "native/atbp-linux-arm64" "native/atbp-darwin-arm64")
if [[ "$TAG" =~ ^v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$ ]]; then
  PRE=()
else
  PRE=(--prerelease)
fi

if gh release view "$TAG" >/dev/null 2>&1; then
  gh release upload "$TAG" "${assets[@]}" --clobber
else
  gh release create "$TAG" "${assets[@]}" \
    --title "$TAG" \
    --generate-notes \
    "${PRE[@]}"
fi"""

val nativeGraal =
  JavaSpec.graalvm(Graalvm.Distribution("graalvm-community"), "25")

val nativeToolsScript =
  s"""set -euo pipefail
if [[ "$${{ matrix.os }}" == "macos-latest" ]]; then
  brew install graphviz
else
  sudo apt-get update
  sudo apt-get install -y graphviz
fi
D2_VERSION=${Dependencies.Versions.D2}
case "$${{ matrix.os }}" in
  ubuntu-24.04) D2_TARBALL="d2-$${D2_VERSION}-linux-amd64.tar.gz" ;;
  ubuntu-24.04-arm) D2_TARBALL="d2-$${D2_VERSION}-linux-arm64.tar.gz" ;;
  macos-latest) D2_TARBALL="d2-$${D2_VERSION}-macos-arm64.tar.gz" ;;
  *) echo "no d2 archive for $${{ matrix.os }}" >&2; exit 1 ;;
esac
D2_URL="https://github.com/terrastruct/d2/releases/download/$${D2_VERSION}"
curl -fsSLO "$$D2_URL/$$D2_TARBALL"
curl -fsSLO "$$D2_URL/SHA256SUMS"
if command -v sha256sum >/dev/null 2>&1; then
  grep " $$D2_TARBALL$$" SHA256SUMS | sha256sum -c -
else
  grep " $$D2_TARBALL$$" SHA256SUMS | shasum -a 256 -c -
fi
mkdir -p "$$HOME/.local/bin"
tar xzf "$$D2_TARBALL" "d2-$${D2_VERSION}/bin/d2"
mv "d2-$${D2_VERSION}/bin/d2" "$$HOME/.local/bin/d2"
chmod +x "$$HOME/.local/bin/d2"
echo "$$HOME/.local/bin" >> "$$GITHUB_PATH"
export PATH="$$HOME/.local/bin:$$PATH"
command -v d2
command -v dot
d2 --version
dot -V
rm -rf "$$D2_TARBALL" SHA256SUMS "d2-$${D2_VERSION}" """

val nativeSmokeScript =
  """set -euo pipefail
ver=$(sbt --client "print version" | perl -pe 's/\e\[[0-9;]*[A-Za-z]//g' | awk '
  /^version$/ { grab = 1; next }
  grab {
    gsub(/^[[:space:]]+|[[:space:]]+$/, "")
    if ($0 != "") { print; exit }
  }
')
if [[ -z "$ver" || ! "$ver" =~ ^[0-9A-Za-z][0-9A-Za-z.+-]*$ ]]; then
  echo "refusing unexpected version token: '$ver'" >&2
  exit 1
fi
bash cli/native-smoke/smoke.sh cli/target/graalvm-native-image/atbp "v${ver}" """

val nativeAssetScript =
  """set -euo pipefail
case "${{ matrix.os }}" in
  ubuntu-24.04) name=atbp-linux-amd64 ;;
  ubuntu-24.04-arm) name=atbp-linux-arm64 ;;
  macos-latest) name=atbp-darwin-arm64 ;;
  *) echo "unexpected runner ${{ matrix.os }}" >&2; exit 1 ;;
esac
src=cli/target/graalvm-native-image/atbp
if [[ ! -f "$src" ]]; then
  echo "missing native image $src" >&2
  exit 1
fi
chmod +x "$src"
cp "$src" "$name"
echo "name=$name" >> "$GITHUB_OUTPUT"
echo "file=$name" >> "$GITHUB_OUTPUT" """

ThisBuild / githubWorkflowAddedJobs := Seq(
  WorkflowJob(
    id = "native",
    name = "Native image",
    steps = List(
      WorkflowStep.CheckoutFull,
      WorkflowStep.SetupJava(List(nativeGraal)).head,
      WorkflowStep.SetupSbt(),
      WorkflowStep.Use(
        UseRef.Public("actions", "download-artifact", "v8"),
        name = Some("Download build targets"),
        params = Map(
          "name" -> s"target-ubuntu-latest-${scalaVersion.value}-temurin@25"
        )
      ),
      WorkflowStep.Run(
        commands = List("tar xf targets.tar", "rm targets.tar"),
        name = Some("Inflate build targets")
      ),
      WorkflowStep.Sbt(
        commands = List("cli/GraalVMNativeImage/packageBin"),
        name = Some("Build native image")
      ),
      WorkflowStep.Run(
        commands = List(nativeToolsScript),
        name = Some("Install Graphviz and d2")
      ),
      WorkflowStep.Run(
        commands = List(nativeSmokeScript),
        name = Some("Smoke test native image")
      ),
      WorkflowStep.Run(
        commands = List(nativeAssetScript),
        id = Some("asset"),
        name = Some("Name the release asset"),
        cond = Some("startsWith(github.ref, 'refs/tags/v')")
      ),
      WorkflowStep.Use(
        UseRef.Public("actions", "upload-artifact", "v7"),
        name = Some("Upload native image"),
        cond = Some("startsWith(github.ref, 'refs/tags/v')"),
        params = Map(
          "name" -> "${{ steps.asset.outputs.name }}",
          "path" -> "${{ steps.asset.outputs.file }}",
          "if-no-files-found" -> "error"
        )
      )
    ),
    cond = Some(
      "github.event_name != 'pull_request' && (startsWith(github.ref, 'refs/tags/v') || github.ref == 'refs/heads/main')"
    ),
    env = Map("SBT_OPTS" -> "-Xmx2G"),
    oses = List("ubuntu-24.04", "ubuntu-24.04-arm", "macos-latest"),
    scalas = List(scalaVersion.value),
    javas = List(nativeGraal),
    needs = List("build"),
    matrixFailFast = Some(true),
    timeout = Some(90.minutes)
  ),
  WorkflowJob(
    id = "release",
    name = "Create GitHub Release",
    steps = List(
      WorkflowStep.Use(
        UseRef.Public("actions", "download-artifact", "v8"),
        name = Some("Download universal zip"),
        params = Map(
          "name" -> "atbp",
          "path" -> "dist"
        )
      ),
      WorkflowStep.Use(
        UseRef.Public("actions", "download-artifact", "v8"),
        name = Some("Download atbp-linux-amd64"),
        params = Map(
          "name" -> "atbp-linux-amd64",
          "path" -> "native"
        )
      ),
      WorkflowStep.Use(
        UseRef.Public("actions", "download-artifact", "v8"),
        name = Some("Download atbp-linux-arm64"),
        params = Map(
          "name" -> "atbp-linux-arm64",
          "path" -> "native"
        )
      ),
      WorkflowStep.Use(
        UseRef.Public("actions", "download-artifact", "v8"),
        name = Some("Download atbp-darwin-arm64"),
        params = Map(
          "name" -> "atbp-darwin-arm64",
          "path" -> "native"
        )
      ),
      WorkflowStep.Run(
        commands = List(releaseGhScript),
        name = Some("Create or update GitHub Release"),
        env = Map(
          "GH_REPO" -> "${{ github.repository }}"
        )
      )
    ),
    cond = Some("startsWith(github.ref, 'refs/tags/v')"),
    permissions = Some(
      Permissions.Specify(
        Map(
          PermissionScope.Contents -> PermissionValue.Write
        )
      )
    ),
    oses = List("ubuntu-latest"),
    scalas = List(scalaVersion.value),
    javas = List(JavaSpec.temurin("25")),
    needs = List("publish", "native")
  )
)
