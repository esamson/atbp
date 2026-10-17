package ph.samson.atbp.md2c

import better.files.File
import zio.ZIO
import zio.test.*

object OfflineRenderSpec extends ZIOSpecDefault {

  private def dirWith(files: (String, String)*): ZIO[Any, Throwable, File] =
    ZIO.attemptBlocking {
      val dir = File.newTemporaryDirectory("offline-render")
      files.foreach { (name, body) =>
        val file = dir / name
        file.parent.createDirectoryIfNotExists(createParents = true)
        file.writeText(body)
      }
      dir
    }

  private val onePlantUml =
    """```plantuml
      |Alice -> Bob: hello
      |```
      |""".stripMargin

  private val twoPlantUml =
    s"""$onePlantUml
       |
       |$onePlantUml
       |""".stripMargin

  private val invalidMermaid =
    """```mermaid
      |invalid!!!
      |```
      |""".stripMargin

  override def spec = suite("OfflineRender")(
    test("a directory with no markdown files fails") {
      for {
        source <- dirWith("notes.txt" -> "nope")
        target <- ZIO.attemptBlocking(File.newTemporaryDirectory("offline-out"))
        error <- OfflineRender.render(source, target).flip
      } yield error match {
        case e: IllegalArgumentException =>
          assertTrue(e.getMessage.contains("no markdown files"))
        case _ => assertTrue(false)
      }
    },
    test("markdown nested in a subdirectory does not count") {
      for {
        source <- dirWith("nested/page.md" -> onePlantUml)
        target <- ZIO.attemptBlocking(File.newTemporaryDirectory("offline-out"))
        error <- OfflineRender.render(source, target).flip
      } yield assertTrue(error.getMessage.contains("no markdown files"))
    },
    test("a dotfile markdown is ignored") {
      for {
        source <- dirWith(".hidden.md" -> onePlantUml)
        target <- ZIO.attemptBlocking(File.newTemporaryDirectory("offline-out"))
        error <- OfflineRender.render(source, target).flip
      } yield assertTrue(error.getMessage.contains("no markdown files"))
    },
    test("a second fence of the same kind fails") {
      for {
        source <- dirWith("a.md" -> twoPlantUml)
        target <- ZIO.attemptBlocking(File.newTemporaryDirectory("offline-out"))
        error <- OfflineRender.render(source, target).flip
      } yield error match {
        case e: IllegalArgumentException =>
          assertTrue(e.getMessage.contains("multiple plantuml"))
        case _ => assertTrue(false)
      }
    },
    test("the same kind split across two files fails") {
      for {
        source <- dirWith(
          "a.md" -> onePlantUml,
          "b.md" -> onePlantUml
        )
        target <- ZIO.attemptBlocking(File.newTemporaryDirectory("offline-out"))
        error <- OfflineRender.render(source, target).flip
      } yield assertTrue(error.getMessage.contains("multiple plantuml"))
    },
    test("a mermaid fence that does not render fails") {
      for {
        source <- dirWith("bad.md" -> invalidMermaid)
        target <- ZIO.attemptBlocking(File.newTemporaryDirectory("offline-out"))
        error <- OfflineRender.render(source, target).flip
      } yield error match {
        case e: IllegalArgumentException =>
          assertTrue(
            e.getMessage.contains("Mermaid rendering failed"),
            !(target / "mermaid.png").exists
          )
        case _ => assertTrue(false)
      }
    }
  )
}
