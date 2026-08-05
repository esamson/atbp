import sbt.*
import sbt.Keys.*
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*

object Dependencies {

  object Versions {
    val AdfBuilderJava = "2.6.0"
    val CommonMark = "0.29.0"
    val Zio = "2.1.26"
    val ZioConfig = "4.0.8"
    val ZioHttp = "3.11.3"
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

    val automaton = "dk.brics" % "automaton" % "1.12-4"

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

    val commonsIo = "commons-io" % "commons-io" % "2.22.0"

    val fastparse = "com.lihaoyi" %% "fastparse" % "3.1.1"

    val glicko2 =
      Def.setting("com.github.mrdimosthenis" %%% "glicko2" % "1.0.1")

    val jsoup = "org.jsoup" % "jsoup" % "1.22.2"

    val laminar = Def.setting("com.raquo" %%% "laminar" % "17.2.1")

    val pdfbox = "org.apache.pdfbox" % "pdfbox" % "3.0.8"

    val plantuml = "net.sourceforge.plantuml" % "plantuml" % "1.2026.6"

    val pprint = "com.lihaoyi" %% "pprint" % "0.9.6"

    val scalaCsv = "com.github.tototoshi" %% "scala-csv" % "2.0.0"

    val zio = "dev.zio" %% "zio" % Zio

    val zioCli = "dev.zio" %% "zio-cli" % "0.8.1"

    val zioConfig = "dev.zio" %% "zio-config" % ZioConfig
    val zioConfigMagnolia = "dev.zio" %% "zio-config-magnolia" % ZioConfig
    val zioConfigTypesafe = "dev.zio" %% "zio-config-typesafe" % ZioConfig
    val zioConfigYaml = "dev.zio" %% "zio-config-yaml" % ZioConfig

    val zioHttp = "dev.zio" %% "zio-http" % ZioHttp

    val zioJson = Def.setting("dev.zio" %%% "zio-json" % "0.10.0")

    val zioTestJs = Def.setting("dev.zio" %%% "zio-test" % Zio % Test)
    val zioTestSbtJs = Def.setting("dev.zio" %%% "zio-test-sbt" % Zio % Test)

    val zioLogging = "dev.zio" %% "zio-logging" % ZioLogging
    val zioLoggingSlf4j2 = "dev.zio" %% "zio-logging-slf4j2" % ZioLogging

    val zioSchemaJson = "dev.zio" %% "zio-schema-json" % "1.8.5"

    object TestLibs {
      val pprint = Libs.pprint % Test

      val slf4jSimple = "org.slf4j" % "slf4j-simple" % "2.0.18"

      val zioConfigTypesafe = Libs.zioConfigTypesafe % Test

      val zioHttpTestkit = "dev.zio" %% "zio-http-testkit" % ZioHttp % Test

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

  val confluence = libraryDependencies ++= Seq(
    betterFiles,
    zio,
    zioHttp,
    zioJson.value,
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

  val hubad = libraryDependencies ++= Seq(
    automaton,
    basedir,
    pdfbox,
    zio,
    TestLibs.slf4jSimple,
    TestLibs.zioLogging,
    TestLibs.zioLoggingSlf4j2,
    TestLibs.zioTest,
    TestLibs.zioTestMagnolia,
    TestLibs.zioTestSbt
  )

  val jira = libraryDependencies ++= Seq(
    zio,
    zioHttp,
    zioJson.value,
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

  val stmt2csv = libraryDependencies ++= Seq(
    zio,
    betterFiles,
    fastparse,
    pdfbox,
    scalaCsv
  )

  val stmt2csvIt = libraryDependencies ++= Seq(
    basedir,
    TestLibs.zioTest,
    TestLibs.zioTestMagnolia,
    TestLibs.zioTestSbt
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

  val liga = libraryDependencies ++= Seq(
    betterFiles,
    glicko2.value,
    zio,
    zioConfig,
    zioConfigMagnolia,
    zioConfigTypesafe,
    zioHttp,
    zioJson.value,
    TestLibs.slf4jSimple,
    TestLibs.zioHttpTestkit,
    TestLibs.zioLogging,
    TestLibs.zioLoggingSlf4j2,
    TestLibs.zioTest,
    TestLibs.zioTestMagnolia,
    TestLibs.zioTestSbt
  )

  val ligaCommon = libraryDependencies ++= Seq(
    glicko2.value,
    zioJson.value,
    TestLibs.zioTest,
    TestLibs.zioTestSbt
  )

  val ligaJs = libraryDependencies ++= Seq(
    glicko2.value,
    laminar.value,
    zioJson.value,
    zioTestJs.value,
    zioTestSbtJs.value
  )
}
