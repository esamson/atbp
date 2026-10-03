semanticdbEnabled := true
semanticdbVersion := scalafixSemanticdb.revision

val fixupCommands =
  List(
    "scalafixAll",
    "scalafixAll OrganizeImports",
    "scalafmtSbt",
    "scalafmtAll"
  )
addCommandAlias("fixup", fixupCommands.mkString("; "))
