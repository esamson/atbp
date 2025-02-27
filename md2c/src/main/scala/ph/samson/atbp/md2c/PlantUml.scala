package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.model.node.CodeBlock
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Media
import com.atlassian.adf.model.node.Media.externalMedia
import com.atlassian.adf.model.node.MediaSingle
import com.atlassian.adf.model.node.MediaSingle.mediaSingle
import com.atlassian.adf.model.node.RichMedia
import com.atlassian.adf.model.node.`type`.DocContent
import net.sourceforge.plantuml.FileFormat
import net.sourceforge.plantuml.FileFormatOption
import net.sourceforge.plantuml.SourceStringReader
import zio.Task
import zio.ZIO

import java.io.ByteArrayOutputStream
import scala.jdk.FunctionConverters.*
import scala.jdk.StreamConverters.*

object PlantUml {

  def render(adf: Doc): ZIO[Any, Throwable, Map[String, File]] = {
    def renders(outDir: File) = adf
      .allNodesOfType(classOf[CodeBlock])
      .filter(codeBlock =>
        codeBlock.language().orElse("").startsWith("plantuml")
      )
      .toScala(List)
      .zipWithIndex
      .map { (codeBlock, index) =>
        val source = codeBlock.toPlainText
        val plantuml =
          if (source.trim.startsWith("@start")) source
          else s"""@startuml
                  |$source
                  |@enduml""".stripMargin

        val fileFormat = codeBlock
          .language()
          .map(l =>
            if (l.contains(".svg")) FileFormat.SVG
            else if (l.contains(".png")) FileFormat.PNG
            else FileFormat.SVG
          )
          .orElse(FileFormat.SVG)
        for {
          out <- ZIO.attemptBlocking {
            val outFile =
              outDir / s"fig-${index + 1}.plantuml${fileFormat.getFileSuffix}"
            val os = new ByteArrayOutputStream()
            val ssr = new SourceStringReader(plantuml)
            ssr.outputImage(os, new FileFormatOption(fileFormat))
            outFile.writeByteArray(os.toByteArray)
          }
        } yield source -> out
      }

    for {
      outDir <- ZIO.attemptBlocking(File.newTemporaryDirectory())
      rendered <- ZIO.collectAllPar(renders(outDir)).map(_.toMap)
    } yield rendered
  }

  def transform(adf: Doc): Task[Doc] = {
    def transformer(renders: Map[String, File]): DocContent => DocContent = {
      case codeBlock: CodeBlock =>
        if (codeBlock.language().orElse("").startsWith("plantuml")) {
          val source = codeBlock.toPlainText
          val path = renders(source).pathAsString
          mediaSingle(RichMedia.Layout.FULL_WIDTH, externalMedia(path))
        } else {
          codeBlock
        }
      case other => other
    }

    for {
      renders <- render(adf)
      xfrm = transformer(renders).asJavaFunction
      _ = adf.transformDescendants(classOf[DocContent], xfrm)
    } yield {
      adf
    }
  }
}
