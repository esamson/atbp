package ph.samson.atbp.confluence.model

import com.atlassian.adf.jackson2.AdfJackson2
import com.atlassian.adf.model.node.Doc

case class PageBodyWrite(
    representation: String,
    value: String
)

object PageBodyWrite {
  private val parser = new AdfJackson2()
  def apply(doc: Doc): PageBodyWrite = PageBodyWrite(
    representation = "atlas_doc_format",
    value = parser.marshall(doc)
  )
}
