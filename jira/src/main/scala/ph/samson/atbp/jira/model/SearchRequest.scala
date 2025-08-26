package ph.samson.atbp.jira.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

case class SearchRequest(
    jql: String,
    fields: List[String],
    maxResults: Int = 50,
    nextPageToken: Option[String] = None
)

object SearchRequest {
  implicit val schema: Schema[SearchRequest] = DeriveSchema.gen
  implicit val codec: BinaryCodec[SearchRequest] =
    JsonCodec.schemaBasedBinaryCodec
}
