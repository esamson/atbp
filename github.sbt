// Restored ThisBuild scoping: sbt 2.0.8 / sbt#9674 was insufficient for
// sbt-github-actions keys; bare settings leaked plugin-default JDK 8
// into generated CI (or failed load/compile as recorded below).
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
