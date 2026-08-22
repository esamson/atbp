import com.typesafe.sbt.packager.rpm.RpmPlugin.autoImport._

addCommandAlias("makeZip", "show cli / Universal / packageBin")
addCommandAlias("dockerPublish", "cli / Docker / publish")

// sbt-native-packager registers Debian/Rpm keys via JavaAppPackaging but they
// are unused when only Docker/Universal are built; sbt 2 lintUnused flags them
// (https://github.com/sbt/sbt-native-packager/issues/1761). Lint reads
// excludeLintKeys at Global scope and matches on key label.
Global / excludeLintKeys += executableScriptName
Global / excludeLintKeys += sourceDirectory
Global / excludeLintKeys += name
Global / excludeLintKeys += daemonStdoutLogFile
Global / excludeLintKeys += rpmScriptsDirectory
