package ph.samson.atbp.jira.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

case class PageOfComments(
    startAt: Int,
    maxResults: Int,
    total: Int,
    comments: List[Comment]
)

object PageOfComments {

  import Schemas.*
  implicit val schema: Schema[PageOfComments] = DeriveSchema.gen
  implicit val codec: BinaryCodec[PageOfComments] =
    JsonCodec.schemaBasedBinaryCodec
}
