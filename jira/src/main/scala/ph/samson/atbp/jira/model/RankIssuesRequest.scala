package ph.samson.atbp.jira.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

/** @param issues
  *   issues to be ranked
  * @param rankAfterIssue
  *   rank issues below this issue
  * @param rankBeforeIssue
  *   rank issues above this issue
  * @param rankCustomFieldId
  *   use this field for ranking
  * @see
  *   https://developer.atlassian.com/cloud/jira/software/rest/api-group-issue/#api-rest-agile-1-0-issue-rank-put
  */
case class RankIssuesRequest(
    issues: List[String],
    rankAfterIssue: Option[String],
    rankBeforeIssue: Option[String],
    rankCustomFieldId: Option[Int]
) {
  require(issues.length <= 50, "At most 50 issues may be ranked at once.")
  require(
    rankAfterIssue.isDefined || rankBeforeIssue.isDefined,
    "At least one of `rankAfterIssue` or `rankBeforeIssue` must be specified."
  )
  require(
    !(rankAfterIssue.isDefined && rankBeforeIssue.isDefined),
    "Only one of `rankAfterIssue` or `rankBeforeIssue` must be specified."
  )
}

object RankIssuesRequest {
  implicit val schema: Schema[RankIssuesRequest] = DeriveSchema.gen
  implicit val codec: BinaryCodec[RankIssuesRequest] =
    JsonCodec.schemaBasedBinaryCodec
}
