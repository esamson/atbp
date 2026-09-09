package ph.samson.atbp.md2c

import com.atlassian.adf.model.node.CodeBlock
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Node

import scala.jdk.CollectionConverters.*
import scala.jdk.FunctionConverters.*

/** Shared soft-failure presentation for diagram renderers (Mermaid, D2, …):
  * inserts a `text` `CodeBlock` sibling carrying the failure details right
  * after each failed diagram `CodeBlock`, wherever it lives in the tree.
  */
object DiagramFailure {

  private[md2c] def insertAfterParentIndex(
      parent: Node,
      index: Int,
      node: CodeBlock
  ): Unit = {
    parent.getClass.getMethod("content").invoke(parent) match {
      case content: java.util.List[?] @unchecked =>
        val updated = new java.util.ArrayList[AnyRef](content)
        updated.add(index + 1, node)
        parent.getClass
          .getMethod("replaceContent", classOf[java.util.List[?]])
          .invoke(parent, updated)
        ()
      case _ =>
        ()
    }
  }

  def insertFailureSiblings(
      adf: Doc,
      errorByBlock: Map[CodeBlock, String],
      detailsHeader: String
  ): Unit =
    if (errorByBlock.nonEmpty) {
      val failedBlocks = errorByBlock.keySet
      val predicate = (codeBlock: CodeBlock) => failedBlocks.contains(codeBlock)
      val insertions = adf
        .findMatchingDescendants(classOf[CodeBlock], predicate.asJavaPredicate)
        .asScala
        .toList
        .flatMap { container =>
          container.children().asScala.toList.map { childMatch =>
            (container.parent(), childMatch.index(), childMatch.`match`())
          }
        }
        .sortBy(-_._2)

      insertions.foreach { (parent, index, codeBlock) =>
        val errorMessage = errorByBlock(codeBlock)
        val errorBlock = CodeBlock
          .codeBlock(s"$detailsHeader\n$errorMessage")
          .language("text")
        insertAfterParentIndex(parent, index, errorBlock)
      }
    }
}
