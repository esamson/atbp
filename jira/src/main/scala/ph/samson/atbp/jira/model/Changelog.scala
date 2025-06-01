package ph.samson.atbp.jira.model

import zio.schema.*
import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

import java.time.ZonedDateTime

import Changelog.*

case class Changelog(
    id: String,
    author: UserDetails,
    created: ZonedDateTime,
    items: List[ChangeDetails],
    historyMetadata: Option[HistoryMetadata]
)

object Changelog {

  case class ChangeDetails(
      field: String,
      fieldType: Option[String],
      fieldId: Option[String],
      from: Option[String],
      fromString: Option[String],
      to: Option[String],
      toAsString: Option[String]
  )

  case class HistoryMetadata(
      activityDescription: Option[String],
      activityDescriptionKey: Option[String],
      description: Option[String],
      descriptionKey: Option[String],
      emailDescription: Option[String],
      emailDescriptionKey: Option[String]
  )

  import Schemas.*
  implicit val detailSchema: Schema[ChangeDetails] =
    Schema.CaseClass7[
      String,
      Option[String],
      Option[String],
      Option[String],
      Option[String],
      Option[String],
      Option[String],
      ChangeDetails
    ](
      id0 = TypeId.fromTypeName("ChangeDetails"),
      field01 = Schema.Field(
        name0 = "field",
        schema0 = Schema[String],
        get0 = _.field,
        set0 = (details, value) => details.copy(field = value)
      ),
      field02 = Schema.Field(
        name0 = "fieldType",
        schema0 = Schema[Option[String]],
        get0 = _.fieldType,
        set0 = (details, value) => details.copy(fieldType = value)
      ),
      field03 = Schema.Field(
        name0 = "fieldId",
        schema0 = Schema[Option[String]],
        get0 = _.fieldId,
        set0 = (details, value) => details.copy(fieldId = value)
      ),
      field04 = Schema.Field(
        name0 = "from",
        schema0 = Schema[Option[String]],
        get0 = _.from,
        set0 = (details, value) => details.copy(from = value)
      ),
      field05 = Schema.Field(
        name0 = "fromString",
        schema0 = Schema[Option[String]],
        get0 = _.fromString,
        set0 = (details, value) => details.copy(fromString = value)
      ),
      field06 = Schema.Field(
        name0 = "to",
        schema0 = Schema[Option[String]],
        get0 = _.to,
        set0 = (details, value) => details.copy(to = value)
      ),
      field07 = Schema.Field(
        name0 = "toString",
        schema0 = Schema[Option[String]],
        get0 = _.toAsString,
        set0 = (details, value) => details.copy(toAsString = value)
      ),
      construct0 = (
          field,
          fieldType,
          fieldId,
          from,
          fromString,
          to,
          toAsString
      ) =>
        ChangeDetails(
          field,
          fieldType,
          fieldId,
          from,
          fromString,
          to,
          toAsString
        )
    )
  implicit val schema: Schema[PageBean[Changelog]] = DeriveSchema.gen
  implicit val codec: BinaryCodec[PageBean[Changelog]] =
    JsonCodec.schemaBasedBinaryCodec
}
