package ph.samson.atbp.md2c

import com.atlassian.adf.model.node.Node
import com.atlassian.adf.plaintext.PlainTextTransformer

private val plainTextTransformer = new PlainTextTransformer()

extension (node: Node) {
  private[md2c] def plainText: String = plainTextTransformer.toPlainText(node)
}
