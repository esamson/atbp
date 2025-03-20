package ph.samson.atbp.jira.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

case class RankIssuesRequest(
    issues: List[String],
    rankAfterIssue: Option[String],
    rankBeforeIssue: Option[String],
    rankCustomFieldId: Option[Int]
)

object RankIssuesRequest {
  implicit val schema: Schema[RankIssuesRequest] = DeriveSchema.gen
  implicit val codec: BinaryCodec[RankIssuesRequest] =
    JsonCodec.schemaBasedBinaryCodec
}
