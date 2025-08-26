package ph.samson.atbp.jira.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

case class SearchResults(
    isLast: Boolean,
    nextPageToken: Option[String],
    issues: List[Issue]
) {
  def length = issues.length
}

object SearchResults {
  implicit val schema: Schema[SearchResults] = DeriveSchema.gen
  implicit val codec: BinaryCodec[SearchResults] =
    JsonCodec.schemaBasedBinaryCodec
}
