ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("21"))

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.StartsWith(Ref.Tag("v")),
  RefPredicate.Equals(Ref.Branch("main"))
)

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
