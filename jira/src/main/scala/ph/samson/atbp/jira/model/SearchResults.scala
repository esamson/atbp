package ph.samson.atbp.jira.model

import ph.samson.atbp.jira.Conf.CustomFields
import zio.Task
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
  def schema(customFields: CustomFields): Task[Schema[SearchResults]] = {
    for {
      given Schema[Issue] <- Issue.schema(customFields)
    } yield DeriveSchema.gen
  }
  def codec(customFields: CustomFields): Task[BinaryCodec[SearchResults]] = {
    for {
      given Schema[SearchResults] <- schema(customFields)

    } yield JsonCodec.schemaBasedBinaryCodec
  }
}
