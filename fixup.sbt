ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

val fixupCommands =
  List(
    "scalafixAll",
    "scalafixAll OrganizeImports",
    "scalafmtSbt",
    "scalafmtAll"
  )
addCommandAlias("fixup", fixupCommands.mkString("; "))
