package ph.samson.atbp.jira.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

case class SearchResults(
    startAt: Int,
    maxResults: Int,
    total: Int,
    issues: List[Issue]
) {
  def length = issues.length
}

object SearchResults {
  implicit val schema: Schema[SearchResults] = DeriveSchema.gen
  implicit val codec: BinaryCodec[SearchResults] =
    JsonCodec.schemaBasedBinaryCodec
}
