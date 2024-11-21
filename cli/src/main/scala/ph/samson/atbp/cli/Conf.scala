package ph.samson.atbp.cli

import ph.samson.atbp.confluence.Conf as ConfluenceConf
import ph.samson.atbp.jira.Conf as JiraConf
import ph.samson.atbp.md2c.Conf as Md2cConf
import ph.samson.xdg.basedir.*
import zio.Config
import zio.ConfigProvider
import zio.Task
import zio.ZIO
import zio.config.magnolia.*
import zio.config.typesafe.*

case class Conf(
    confluence: Option[ConfluenceConf],
    jira: Option[JiraConf],
    md2c: Option[Md2cConf]
) {
  def jiraConf: Option[JiraConf] = jira.orElse(confluence.map {
    case ConfluenceConf(site, user, token) => JiraConf(site, user, token)
  })

  def confluenceConf: Option[ConfluenceConf] = confluence.orElse(jira.map {
    case JiraConf(site, user, token) => ConfluenceConf(site, user, token)
  })
}

object Conf {
  val Descriptor: Config[Conf] = deriveConfig[Conf]

  val appConf: Task[Conf] = for {
    config <- ZIO.attemptBlockingIO(config("atbp").load)
    provider <- ConfigProvider.fromTypesafeConfigZIO(config.getConfig("atbp"))
    conf <- provider.load(Descriptor)
  } yield conf
}
