package ph.samson.atbp.confluence.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

import java.time.ZonedDateTime

import CreateAttachmentResponse.*

case class CreateAttachmentResponse(
    results: List[Content],
    start: Option[Int],
    limit: Option[Int],
    size: Int
)

object CreateAttachmentResponse {

  case class Content(
      id: String,
      `type`: String,
      status: String,
      title: String,
      version: Version,
      container: Container,
      metadata: Metadata,
      extensions: Extensions
  )

  case class Version(
      by: User,
      when: ZonedDateTime,
      friendlyWhen: String,
      message: String,
      number: Int,
      minorEdit: Boolean,
      contentTypeModified: Boolean
  )
  case class User(
      `type`: String,
      accountId: String,
      accountType: String,
      email: String,
      publicName: String,
      timeZone: String,
      displayName: String,
      isExternalCollaborator: Boolean,
      isGuest: Boolean
  )

  case class Container(
      id: String,
      `type`: String,
      status: String,
      title: String
  )

  case class Metadata(
      comment: String,
      mediaType: String
  )

  case class Extensions(
      mediaType: String,
      fileSize: Int,
      comment: String,
      mediaTypeDescription: Option[String],
      fileId: String,
      collectionName: String
  )

  implicit val schema: Schema[CreateAttachmentResponse] = DeriveSchema.gen
  implicit val codec: BinaryCodec[CreateAttachmentResponse] =
    JsonCodec.schemaBasedBinaryCodec
}
