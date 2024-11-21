package ph.samson.atbp.confluence

import better.files.*
import zio.Runtime
import zio.System
import zio.ZIO
import zio.ZLayer
import zio.config.typesafe.TypesafeConfigProvider
import zio.http
import zio.logging.consoleLogger
import zio.test.ZIOSpec

import java.io.StringReader
import java.util.Properties
import scala.jdk.CollectionConverters.*

abstract class ConfluenceSpec extends ZIOSpec[Client] {

  object EnvVars {
    val Site = "CONFLUENCE_SITE"
    val User = "CONFLUENCE_USER"
    val Token = "CONFLUENCE_TOKEN"
  }

  object SysProps {
    val Site = "atbp.confluence.site"
    val User = "atbp.confluence.user"
    val Token = "atbp.confluence.token"
  }

  val TestStrings = for {
    env <- ZIO.attemptBlocking(Resource.asString("confluence.env"))
    props <- ZIO.attempt(env.map { e =>
      val p = new Properties()
      p.load(new StringReader(e))
      p
    })
  } yield {
    val confMap = props.map(_.asScala).getOrElse(Map.empty[String, String])
    for {
      site <- confMap.get(EnvVars.Site)
      user <- confMap.get(EnvVars.User)
      token <- confMap.get(EnvVars.Token)
    } {
      java.lang.System.setProperty(SysProps.Site, site)
      java.lang.System.setProperty(SysProps.User, user)
      java.lang.System.setProperty(SysProps.Token, token)
    }
    confMap
  }

  val conf = ZLayer {
    for {
      _ <- TestStrings
      site <- System.propertyOrElse(SysProps.Site, "bogus")
      user <- System.propertyOrElse(SysProps.User, "bogus")
      token <- System.propertyOrElse(SysProps.Token, "bogus")
    } yield {
      Conf(site, user, token)
    }
  }

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

  override def bootstrap =
    (http.Client.default ++ logger) >>> conf.flatMap(e => Client.layer(e.get))
}
