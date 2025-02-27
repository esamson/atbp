package ph.samson.atbp.confluence.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

import UpdatePageRequest.*

case class UpdatePageRequest(
    id: String,
    status: String,
    title: String,
    spaceId: Option[String] = None,
    parentId: Option[String] = None,
    ownerId: Option[String] = None,
    body: PageBodyWrite,
    version: Version
)

object UpdatePageRequest {

  case class Version(number: Int, message: Option[String] = None)

  def from(page: PageSingle, message: String) = UpdatePageRequest(
    id = page.id,
    status = "current",
    title = page.title,
    spaceId = Some(page.spaceId),
    parentId = page.parentId,
    ownerId = Some(page.ownerId),
    body = PageBodyWrite(
      page.body.storage.map(_.representation).getOrElse("storage"),
      page.body.storage.map(_.value).getOrElse("EMPTY")
    ),
    version = if (page.status == "draft" ) {
      Version(1, Some(message))
    } else {
      Version(page.version.number + 1, Some(message))
    }
  )

  implicit val schema: Schema[UpdatePageRequest] = DeriveSchema.gen
  implicit val codec: BinaryCodec[UpdatePageRequest] =
    JsonCodec.schemaBasedBinaryCodec
}
