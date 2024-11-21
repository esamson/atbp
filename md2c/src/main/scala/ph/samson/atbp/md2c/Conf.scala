package ph.samson.atbp.md2c

import zio.Task
import zio.ZIO

/** Default, user-level configuration
  *
  * @param spaceKey
  */
case class Conf(spaceKey: Option[String]) {

  def finalConf(sourceConf: SourceConf): Task[Conf.Final] = for {
    spaceKey <- ZIO.getOrFailWith(new NoSuchElementException("spaceKey"))(
      sourceConf.spaceKey.orElse(spaceKey)
    )
    pageId <- ZIO.getOrFailWith(new NoSuchElementException("pageId"))(
      sourceConf.pageId
    )
  } yield Conf.Final(spaceKey, pageId)
}

object Conf {

  val Empty: Conf = Conf(None)

  case class Final(
      spaceKey: String,
      pageId: String
  )
}
