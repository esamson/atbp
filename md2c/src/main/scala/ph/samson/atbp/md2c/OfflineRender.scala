package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.model.node.CodeBlock
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Media.ExternalMedia
import zio.Task
import zio.ZIO

import scala.jdk.StreamConverters.*

object OfflineRender {

  val ImageNames: Map[String, String] = Map(
    "plantuml" -> "plantuml.png",
    "mermaid" -> "mermaid.png",
    "d2" -> "d2.png"
  )

  private val tempSuffix: Map[String, String] = Map(
    "plantuml" -> ".plantuml.png",
    "mermaid" -> ".mermaid.png",
    "d2" -> ".d2.png"
  )

  def render(sourceDir: File, targetDir: File): Task[Unit] =
    for {
      files <- markdownFiles(sourceDir)
      _ <- ZIO.when(files.isEmpty)(
        ZIO.fail(
          new IllegalArgumentException(
            s"no markdown files in ${sourceDir.path}"
          )
        )
      )
      parsed <- ZIO.foreach(files)(Parser.parse)
      fences = parsed.flatMap(p => diagramFences(p.doc))
      _ <- rejectDuplicateKinds(fences)
      rendered <- ZIO.foreach(parsed.map(_.doc))(transform)
      _ <- ZIO.attemptBlocking(
        targetDir.createDirectoryIfNotExists(createParents = true)
      )
      kinds = fences.map(_.kind).distinct
      _ <- ZIO.foreachDiscard(kinds) { kind =>
        copyKind(rendered, kind, targetDir / ImageNames(kind))
      }
    } yield ()

  private case class Fence(kind: String, language: String)

  private def markdownFiles(sourceDir: File): Task[List[File]] =
    ZIO.attemptBlocking {
      sourceDir.list
        .filter { file =>
          file.isRegularFile &&
          file.name.endsWith(".md") &&
          !file.name.startsWith(".")
        }
        .toList
        .sortBy(_.name)
    }

  private def kindOf(language: String): Option[String] =
    if (language.startsWith("plantuml")) Some("plantuml")
    else if (language.startsWith("mermaid")) Some("mermaid")
    else if (language.startsWith("d2")) Some("d2")
    else None

  private def diagramFences(doc: Doc): List[Fence] =
    doc
      .allNodesOfType(classOf[CodeBlock])
      .toScala(List)
      .flatMap { block =>
        kindOf(block.language().orElse("")).map { kind =>
          Fence(kind, block.language().orElse(""))
        }
      }

  private def rejectDuplicateKinds(fences: List[Fence]): Task[Unit] =
    ZIO.foreachDiscard(fences.groupBy(_.kind)) { case (kind, items) =>
      ZIO.when(items.size > 1)(
        ZIO.fail(
          new IllegalArgumentException(
            s"multiple $kind fences; --render-only writes one ${ImageNames(kind)}"
          )
        )
      )
    }

  private def transform(doc: Doc): Task[Doc] =
    for {
      plant <- PlantUml.transform(doc)
      mermaid <- Mermaid.transform(plant)
      d2 <- D2.transform(mermaid)
    } yield d2

  private def copyKind(docs: List[Doc], kind: String, dest: File): Task[Unit] =
    for {
      leftovers <- ZIO.succeed(
        docs.flatMap { doc =>
          doc
            .allNodesOfType(classOf[CodeBlock])
            .toScala(List)
            .filter(block => kindOf(block.language().orElse("")).contains(kind))
        }
      )
      _ <- ZIO.when(leftovers.nonEmpty)(
        ZIO.fail(
          new IllegalArgumentException(
            s"$kind fence did not produce ${ImageNames(kind)}:\n" +
              leftovers.map(_.toPlainText).mkString("\n")
          )
        )
      )
      medias = docs.flatMap { doc =>
        doc
          .allNodesOfType(classOf[ExternalMedia])
          .toScala(List)
          .filter(_.url().endsWith(tempSuffix(kind)))
      }
      source <- medias match {
        case media :: Nil => ZIO.attemptBlocking(File(media.url()))
        case _            =>
          ZIO.fail(
            new IllegalArgumentException(
              s"$kind fence did not produce ${ImageNames(kind)}"
            )
          )
      }
      size <- ZIO.attemptBlocking(if (source.exists) source.size else 0L)
      _ <- ZIO.when(size == 0L)(
        ZIO.fail(
          new IllegalArgumentException(
            s"$kind fence did not produce ${ImageNames(kind)}"
          )
        )
      )
      _ <- ZIO.attemptBlocking(source.copyTo(dest, overwrite = true))
    } yield ()
}
