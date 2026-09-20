// Restored ThisBuild scoping: sbt 2.0.8 / sbt#9674 was insufficient for
// sbt-github-actions keys; bare settings leaked plugin-default JDK 8
// into generated CI.
// sbt-github-actions reads ThisBuild-scoped keys; sbt 2 bare common
// settings do not populate that scope, which is why Java, Scala matrix,
// and workflow keys need explicit ThisBuild / here.
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("25"))
// Load-bearing: without ThisBuild scalaVersion the publish matrix reverts
// to the plugin default 3.8.4 even when githubWorkflowScalaVersions is pinned.
// scala3V is not in scope in this file (sbt 2 compiles .sbt files separately).
ThisBuild / scalaVersion := "3.9.0"
ThisBuild / githubWorkflowScalaVersions := Seq("3.9.0")

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
