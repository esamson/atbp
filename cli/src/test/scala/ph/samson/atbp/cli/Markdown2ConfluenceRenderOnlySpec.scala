package ph.samson.atbp.cli

import better.files.File
import ph.samson.atbp.md2c.D2
import zio.ZIO
import zio.test.*

import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import javax.imageio.ImageIO

object Markdown2ConfluenceRenderOnlySpec extends ZIOSpecDefault {

  private val emptyConf = Conf(None, None, None, None)

  private val diagrams =
    """```plantuml
      |Alice -> Bob: hello
      |```
      |
      |```mermaid
      |sequenceDiagram
      |    Alice->>Bob: Hello
      |```
      |
      |```d2.png
      |x -> y
      |```
      |""".stripMargin

  private val failureSnippets = List(
    "Mermaid rendering failed",
    "D2 rendering failed",
    "d2 not on PATH"
  )

  private def toolsAvailable =
    for {
      d2 <- D2.d2Available
      dot <- ZIO
        .attemptBlocking {
          val process = new ProcessBuilder("dot", "-V").start()
          process.getOutputStream.close()
          process.waitFor() == 0
        }
        .orElseSucceed(false)
    } yield d2 && dot

  private def captureErr[A](
      effect: ZIO[Any, Throwable, A]
  ): ZIO[Any, Throwable, (A, String)] = {
    val buffer = new ByteArrayOutputStream()
    val original = System.err
    ZIO.acquireReleaseWith(
      ZIO.succeed(System.setErr(new PrintStream(buffer, true, "UTF-8")))
    )(_ => ZIO.succeed(System.setErr(original))) { _ =>
      effect.map(value => value -> buffer.toString("UTF-8"))
    }
  }

  private def png(file: File): BufferedImage =
    ImageIO.read(file.toJava)

  override def spec = suite("md2c --render-only")(
    test("writes the three images without confluence config") {
      for {
        source <- ZIO.attemptBlocking {
          val dir = File.newTemporaryDirectory("render-only-src")
          (dir / "diagrams.md").writeText(diagrams)
          dir
        }
        target <- ZIO.attemptBlocking(
          File.newTemporaryDirectory("render-only-out")
        )
        ((), err) <- captureErr(
          Markdown2Confluence(
            sourceDir = source,
            cleanup = false,
            renderOnly = true,
            target = Some(target)
          ).run(emptyConf)
        )
        images = List("plantuml.png", "mermaid.png", "d2.png").map(target / _)
      } yield assertTrue(
        images.forall(file => file.exists && file.size > 0),
        images.forall { file =>
          val image = png(file)
          image.getWidth > 8 && image.getHeight > 8
        },
        failureSnippets.forall(snippet => !err.contains(snippet))
      )
    }.whenZIO(toolsAvailable),
    test("--render-only and --cleanup together fail") {
      for {
        source <- ZIO.attemptBlocking(
          File.newTemporaryDirectory("render-only-src")
        )
        target <- ZIO.attemptBlocking(
          File.newTemporaryDirectory("render-only-out")
        )
        error <- Markdown2Confluence(
          sourceDir = source,
          cleanup = true,
          renderOnly = true,
          target = Some(target)
        ).run(emptyConf).flip
      } yield error match {
        case e: IllegalArgumentException =>
          assertTrue(
            e.getMessage.contains("--render-only and --cleanup"),
            !e.getMessage.contains("No confluence config")
          )
        case _ => assertTrue(false)
      }
    },
    test("--render-only without --target fails") {
      for {
        source <- ZIO.attemptBlocking(
          File.newTemporaryDirectory("render-only-src")
        )
        error <- Markdown2Confluence(
          sourceDir = source,
          cleanup = false,
          renderOnly = true,
          target = None
        ).run(emptyConf).flip
      } yield error match {
        case e: IllegalArgumentException =>
          assertTrue(e.getMessage.contains("--target"))
        case _ => assertTrue(false)
      }
    },
    test("publish still requires confluence config") {
      for {
        source <- ZIO.attemptBlocking(
          File.newTemporaryDirectory("render-only-src")
        )
        error <- Markdown2Confluence(
          sourceDir = source,
          cleanup = false,
          renderOnly = false,
          target = None
        ).run(emptyConf).flip
      } yield assertTrue(error.getMessage.contains("No confluence config"))
    }
  )
}
