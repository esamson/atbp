package ph.samson.atbp.jira

import zio.Runtime
import zio.System
import zio.ZLayer
import zio.config.typesafe.TypesafeConfigProvider
import zio.http
import zio.logging.consoleLogger
import zio.test.ZIOSpec

abstract class JiraSpec extends ZIOSpec[Client] {

  object EnvVars {
    val Site = "JIRA_SITE"
    val User = "JIRA_USER"
    val Token = "JIRA_TOKEN"
    val Search = "JIRA_QUERY"
  }

  val conf = ZLayer {
    for {
      site <- System.envOrElse(EnvVars.Site, "bogus")
      user <- System.envOrElse(EnvVars.User, "bogus")
      token <- System.envOrElse(EnvVars.Token, "bogus")
    } yield {
      Conf(site, user, token)
    }
  }

  val logger = {
    val configProvider = TypesafeConfigProvider.fromHoconString(
      """logger {
        |  format = "[%level %name %spans] %message"
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
