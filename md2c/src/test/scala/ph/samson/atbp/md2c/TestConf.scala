package ph.samson.atbp.md2c

import ph.samson.atbp.confluence as c
import zio.Config
import zio.ConfigProvider
import zio.Task
import zio.config.magnolia.*
import zio.config.typesafe.*

case class TestConf(md2c: Conf, confluence: c.Conf)

object TestConf {

  val Descriptor: Config[TestConf] = deriveConfig[TestConf]

  def load(): Task[TestConf] = for {
    source <- ConfigProvider.fromResourcePathZIO()
    conf <- source.load(Descriptor)
  } yield conf
}
