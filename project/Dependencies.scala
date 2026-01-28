import sbt.*
import sbt.Keys.*

object Dependencies {

  object Versions {
    val AdfBuilderJava = "2.0.1"
    val CommonMark = "0.27.1"
    val Zio = "2.1.24"
    val ZioConfig = "4.0.6"
    val ZioHttp = "3.7.4"
    val ZioLogging = "2.5.3"
  }

  object Libs {
    import Versions.*

    val adfBuilderJava =
      "com.atlassian.adf" % s"adf-builder-java" % AdfBuilderJava
    val adfBuilderJavaJackson2 =
      "com.atlassian.adf" % s"adf-builder-java-jackson2" % AdfBuilderJava
    val adfBuilderJavaMarkdown =
      "com.atlassian.adf" % s"adf-builder-java-markdown" % AdfBuilderJava

    val basedir = "ph.samson.xdg" %% "basedir" % "1.0"

    val betterFiles = "com.github.pathikrit" %% "better-files" % "3.9.2"

    val commonmark = "org.commonmark" % "commonmark" % CommonMark
    val commonmarkExtGfmStrikethrough =
      "org.commonmark" % "commonmark-ext-gfm-strikethrough" % CommonMark
    val commonmarkExtGfmTables =
      "org.commonmark" % "commonmark-ext-gfm-tables" % CommonMark
    val commonmarkExtImageAttributes =
      "org.commonmark" % "commonmark-ext-image-attributes" % CommonMark
    val commonmarkExtIns = "org.commonmark" % "commonmark-ext-ins" % CommonMark

    val commonsIo = "commons-io" % "commons-io" % "2.21.0"

    val jsoup = "org.jsoup" % "jsoup" % "1.22.1"

    val plantuml = "net.sourceforge.plantuml" % "plantuml" % "1.2026.1"

    val pprint = "com.lihaoyi" %% "pprint" % "0.9.6"

    val scalaCsv = "com.github.tototoshi" %% "scala-csv" % "2.0.0"

    val zio = "dev.zio" %% "zio" % Zio

    val zioCli = "dev.zio" %% "zio-cli" % "0.7.5"

    val zioConfig = "dev.zio" %% "zio-config" % ZioConfig
    val zioConfigMagnolia = "dev.zio" %% "zio-config-magnolia" % ZioConfig
    val zioConfigTypesafe = "dev.zio" %% "zio-config-typesafe" % ZioConfig
    val zioConfigYaml = "dev.zio" %% "zio-config-yaml" % ZioConfig

    val zioHttp = "dev.zio" %% "zio-http" % ZioHttp

    val zioJson = "dev.zio" %% "zio-json" % "0.8.0"

    val zioLogging = "dev.zio" %% "zio-logging" % ZioLogging
    val zioLoggingSlf4j2 = "dev.zio" %% "zio-logging-slf4j2" % ZioLogging

    val zioSchemaJson = "dev.zio" %% "zio-schema-json" % "1.7.6"

    object TestLibs {
      val pprint = Libs.pprint % Test

      val slf4jSimple = "org.slf4j" % "slf4j-simple" % "2.0.17"

      val zioConfigTypesafe = Libs.zioConfigTypesafe % Test

      val zioLogging = Libs.zioLogging % Test
      val zioLoggingSlf4j2 = Libs.zioLoggingSlf4j2 % Test

      val zioTest = "dev.zio" %% "zio-test" % Zio % Test
      val zioTestMagnolia = "dev.zio" %% "zio-test-magnolia" % Zio % Test
      val zioTestSbt = "dev.zio" %% "zio-test-sbt" % Zio % Test
    }
  }

  import Libs.*

  val cli = libraryDependencies ++= Seq(
    basedir,
    pprint,
    zioCli,
    zioConfig,
    zioConfigMagnolia,
    zioConfigTypesafe,
    zioLogging
  )
  // override needed until https://github.com/zio/zio-cli/pull/508
  val cliOverrides = dependencyOverrides += zioJson

  val confluence = libraryDependencies ++= Seq(
    betterFiles,
    zio,
    zioHttp,
    zioJson,
    zioSchemaJson,
    TestLibs.pprint,
    TestLibs.slf4jSimple,
    TestLibs.zioConfigTypesafe,
    TestLibs.zioLogging,
    TestLibs.zioLoggingSlf4j2,
    TestLibs.zioTest,
    TestLibs.zioTestMagnolia,
    TestLibs.zioTestSbt
  )

  val http = libraryDependencies ++= Seq(
    zioHttp
  )

  val jira = libraryDependencies ++= Seq(
    zio,
    zioHttp,
    zioJson,
    zioSchemaJson,
    TestLibs.pprint,
    TestLibs.zioConfigTypesafe,
    TestLibs.zioLogging,
    TestLibs.zioTest,
    TestLibs.zioTestMagnolia,
    TestLibs.zioTestSbt
  )

  val md2c = libraryDependencies ++= Seq(
    zio,
    betterFiles,
    commonmark,
    commonmarkExtGfmStrikethrough,
    commonmarkExtGfmTables,
    commonmarkExtImageAttributes,
    commonmarkExtIns,
    plantuml,
    zioConfig,
    zioConfigMagnolia,
    zioConfigTypesafe,
    zioConfigYaml,
    TestLibs.pprint,
    TestLibs.slf4jSimple,
    TestLibs.zioLogging,
    TestLibs.zioLoggingSlf4j2,
    TestLibs.zioTest,
    TestLibs.zioTestMagnolia,
    TestLibs.zioTestSbt
  )

  val plate = libraryDependencies ++= Seq(
    betterFiles,
    jsoup,
    zio,
    zioConfig,
    zioConfigMagnolia,
    zioConfigTypesafe,
    zioConfigYaml,
    TestLibs.pprint,
    TestLibs.slf4jSimple,
    TestLibs.zioLogging,
    TestLibs.zioLoggingSlf4j2,
    TestLibs.zioTest,
    TestLibs.zioTestMagnolia,
    TestLibs.zioTestSbt
  )

  val retext = libraryDependencies ++= Seq(
    zio,
    betterFiles,
    commonsIo
  )

  val traceviz = libraryDependencies ++= Seq(
    zio,
    betterFiles,
    scalaCsv
  )

  val adfBuilder = libraryDependencies ++= Seq(
    adfBuilderJava,
    adfBuilderJavaJackson2,
    adfBuilderJavaMarkdown
  )
}
