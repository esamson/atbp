package ph.samson.atbp.md2c

import better.files.File
import zio.Config
import zio.ConfigProvider
import zio.Task
import zio.ZIO
import zio.config.magnolia.*
import zio.config.typesafe.*

/** Configuration taken from source tree.
  *
  * @param spaceKey
  *   space where source tree is published.
  * @param pageId
  *   page which will hold the top of the source tree.
  */
case class SourceConf(
    spaceKey: Option[String],
    pageId: Option[String]
)

object SourceConf {
  val FileName = ".md2c.conf"
  val Descriptor: Config[SourceConf] = deriveConfig[SourceConf]

  def get(sourceDir: File): Task[Option[SourceConf]] =
    if ((sourceDir / FileName).isRegularFile)
      for {
        source <- ConfigProvider.fromHoconFileZIO((sourceDir / FileName).toJava)
        conf <- source.load(Descriptor)
      } yield Some(conf)
    else ZIO.none

  def of(sourceDir: File): Task[SourceConf] = get(sourceDir).some.orElseFail(
    new IllegalArgumentException(s"No `.md2c.conf` file for $sourceDir")
  )

  def of(sourceTree: SourceTree): Task[SourceConf] = {
    val sourceDir = if (sourceTree.root.source.isDirectory) {
      sourceTree.root.source
    } else {
      sourceTree.root.source.parent
    }
    of(sourceDir)
  }
}
