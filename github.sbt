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
fi"""

ThisBuild / githubWorkflowAddedJobs := Seq(
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
    needs = List("publish")
  )
)
