package ph.samson.atbp.md2c

import better.files.*
import ph.samson.atbp.confluence.Client
import zio.Runtime
import zio.Task
import zio.ZIO
import zio.ZLayer
import zio.config.typesafe.TypesafeConfigProvider
import zio.http
import zio.logging.consoleLogger
import zio.test.*

object PublisherSpec extends ZIOSpecDefault {

  override def spec = suite("Publisher")(
    test("Single Node") {
      val publish = for {
        dir <- testDir("Single Node")
        publisher <- ZIO.service[Publisher]
        source <- SourceTree.from(dir)
        _ <- ZIO.log(s"source: $source")
        res <- publisher.publish(source)
        _ <- ZIO.log(s"published: $res")
      } yield res

      for (p <- Live.live(publish).exit) yield assertTrue(p.isSuccess)
    },
    test("Single Child") {
      val publish = for {
        dir <- testDir("Single Child")
        publisher <- ZIO.service[Publisher]
        source <- SourceTree.from(dir)
        _ <- ZIO.log(s"source: $source")
        res <- publisher.publish(source)
        _ <- ZIO.log(s"published: $res")
      } yield res

      for (p <- Live.live(publish).exit) yield assertTrue(p.isSuccess)
    },
    test("Multiple Children") {
      val publish = for {
        dir <- testDir("Multiple Children")
        publisher <- ZIO.service[Publisher]
        source <- SourceTree.from(dir)
        _ <- ZIO.log(s"source: $source")
        res <- publisher.publish(source)
        _ <- ZIO.log(s"published: $res")
      } yield res

      for (p <- Live.live(publish).exit) yield assertTrue(p.isSuccess)
    },
    test("Attached Image") {
      val publish = for {
        dir <- testDir("Attached Image")
        publisher <- ZIO.service[Publisher]
        source <- SourceTree.from(dir)
        _ <- ZIO.log(s"source: $source")
        res <- publisher.publish(source)
        _ <- ZIO.log(s"published: $res")
      } yield res

      for (p <- Live.live(publish).exit) yield assertTrue(p.isSuccess)
    }
  ).provideShared(pubLayer)

  def testDir(name: String): Task[File] =
    ZIO.attemptBlocking(TestFiles("publisher") / name)

  val logger = {
    val configProvider = TypesafeConfigProvider.fromHoconString(
      """logger {
        |  format = "[%level %name][%spans][%kvs] %message"
        |  filter {
        |    rootLevel = DEBUG
        |  }
        |}
        |""".stripMargin
    )
    Runtime.removeDefaultLoggers
      >>> Runtime.setConfigProvider(configProvider)
      >>> consoleLogger()
  }

  val testConf = TestConf.load()
  val publisher = ZLayer {
    testConf.map(_.md2c)
  } flatMap (e => Publisher.layer(e.get))

  val confluence =
    ZLayer(testConf.map(_.confluence)).flatMap(e => Client.layer(e.get))

  val confluenceClient =
    (http.Client.default ++ logger) >>> confluence

  val pubLayer = (confluenceClient) >>> publisher
}
