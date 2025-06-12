package ph.samson.atbp.md2c

import com.atlassian.adf.model.node.CodeBlock
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Extension
import com.atlassian.adf.model.node.Text
import com.atlassian.adf.model.node.`type`.DocContent
import com.typesafe.config.ConfigFactory

import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Extensions {

  def transform(adf: Doc) = {

    adf.transformDescendants(
      classOf[DocContent],
      {
        case codeBlock: CodeBlock =>
          if (codeBlock.language().orElse("").equals("extension")) {
            val content: java.util.List[Text] = codeBlock.content()
            val result: DocContent = Try {
              val config = ConfigFactory.parseString(
                content.asScala.map(_.text()).mkString("\n")
              )
              val default = Extension
                .extension()
                .extensionKey(config.getString("extensionKey"))
                .extensionType(config.getString("extensionType"))
              if (config.hasPath("parameters")) {
                default.parameters(config.getObject("parameters").unwrapped())
              } else {
                default
              }
            } match {
              case Success(extension) => extension
              case Failure(exception) =>
                exception.printStackTrace()
                val error = Text.text(exception.getMessage + "\n\n")
                val newContent = error :: content.asScala.toList
                codeBlock.replaceContent(newContent.asJava)
                codeBlock
            }
            result
          } else {
            codeBlock
          }
        case other => other
      }
    )

    adf
  }

}
