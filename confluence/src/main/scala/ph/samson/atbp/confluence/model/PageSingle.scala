package ph.samson.atbp.confluence.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

import java.time.ZonedDateTime

import PageSingle.*

case class PageSingle(
    id: String,
    status: String,
    title: String,
    spaceId: String,
    parentId: Option[String],
    parentType: Option[String],
    position: Option[Int],
    authorId: String,
    ownerId: String,
    lastOwnerId: Option[String],
    createdAt: String,
    version: Version,
    body: BodySingle,
    labels: Option[Labels],
    properties: Option[Properties],
    _links: Links
) {
  def isDraft: Boolean = status == "draft"
}

object PageSingle {
  case class Version(
      createdAt: ZonedDateTime,
      message: String,
      number: Int,
      minorEdit: Boolean,
      authorId: String
  )

  case class BodySingle(
      storage: Option[BodyType],
      atlas_doc_format: Option[BodyType],
      view: Option[BodyType]
  )

  case class BodyType(
      representation: String,
      value: String
  )

  case class Labels(
      results: List[Label],
      meta: OptionalFieldMeta,
      _links: OptionalFieldLinks
  )

  case class Label(
      id: String,
      name: String,
      prefix: String
  )

  case class Properties(
      results: List[ContentProperty],
      meta: OptionalFieldMeta,
      _links: OptionalFieldLinks
  )

  case class ContentProperty(
      id: String,
      key: String,
      value: String,
      version: Version
  )

  case class OptionalFieldMeta(
      hasMore: Boolean,
      cursor: String
  )

  case class OptionalFieldLinks(
      self: String
  )

  case class Links(
      webui: String,
      editui: String,
      tinyui: String
  )

  implicit val schema: Schema[PageSingle] = DeriveSchema.gen
  implicit val codec: BinaryCodec[PageSingle] = JsonCodec.schemaBasedBinaryCodec
}
