ThisBuild / organization := "ph.samson.atbp"
ThisBuild / organizationName := "Edward Samson"
ThisBuild / startYear := Some(2024)
ThisBuild / licenses += "AGPL-3.0-or-later" -> url(
  "https://spdx.org/licenses/AGPL-3.0-or-later.html"
)
ThisBuild / homepage := Some(url("https://github.com/esamson/atbp"))
ThisBuild / developers := List(
  Developer(
    "esamson",
    "Edward Samson",
    "edward@samson.ph",
    url("https://edward.samson.ph")
  )
)

ThisBuild / sonatypeCredentialHost := Sonatype.sonatypeCentralHost
sonatypeProfileName := "ph.samson"
