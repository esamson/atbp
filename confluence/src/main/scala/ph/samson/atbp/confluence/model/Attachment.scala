package ph.samson.atbp.confluence.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

import java.time.ZonedDateTime

import Attachment.*

case class Attachment(
    mediaTypeDescription: Option[String],
    webuiLink: String,
    createdAt: ZonedDateTime,
    comment: String,
    version: Version,
    title: String,
    status: String,
    fileSize: Int,
    fileId: String,
    mediaType: String,
    pageId: String,
    downloadLink: String,
    id: String,
    _links: Links
)

object Attachment {
  case class Version(
      number: Int,
      message: String,
      minorEdit: Boolean,
      authorId: String,
      createdAt: ZonedDateTime
  )

  case class Links(download: String, webui: String)

  implicit val schema: Schema[MultiEntityResult[Attachment]] = DeriveSchema.gen
  implicit val codec: BinaryCodec[MultiEntityResult[Attachment]] =
    JsonCodec.schemaBasedBinaryCodec
}
