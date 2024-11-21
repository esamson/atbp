ThisBuild / organization := "samson.ph"
ThisBuild / scalaVersion := "3.5.2"

lazy val root = Project("atbp", file("."))
  .settings(
    name := "atbp-root",
    publish / skip := true
  )
  .aggregate(cli, confluence, http, jira, md2c, plate)

lazy val cli = atbpModule("cli")
  .dependsOn(md2c, plate)
  .enablePlugins(JavaAppPackaging)
  .settings(Dependencies.cli)
  .settings(
    // sbt-native-packager
    packageName := "atbp",
    maintainer := "edward@samson.ph",
    executableScriptName := packageName.value
  )

lazy val confluence = atbpModule("confluence")
  .dependsOn(adfBuilder, http)
  .settings(Dependencies.confluence)

lazy val http = atbpModule("http")
  .settings(Dependencies.http)

lazy val jira = atbpModule("jira")
  .dependsOn(http)
  .settings(Dependencies.jira)

lazy val md2c = atbpModule("md2c")
  .dependsOn(adfBuilder, confluence)
  .settings(Dependencies.md2c)
  .settings(
    // Don't exclude hidden files so .md2c.conf files show up in tests.
    Test / fileInputExcludeFilter := DirectoryFilter.toNio
  )

lazy val plate = atbpModule("plate")
  .dependsOn(jira)
  .settings(Dependencies.plate)

// Pseudo-project to limit usage of Atlassian repo
lazy val adfBuilder = atbpModule("adf-builder")
  .settings(
    resolvers += "Atlassian Packages" at "https://packages.atlassian.com/artifactory/maven-external",
    Dependencies.adfBuilder
  )

def atbpModule(moduleName: String): Project =
  Project(moduleName, file(moduleName))
    .settings(name := s"atbp-$moduleName")
