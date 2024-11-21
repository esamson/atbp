package ph.samson.atbp.confluence.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

import java.time.ZonedDateTime

case class Space(
    id: String,
    key: String,
    name: String,
    `type`: String,
    authorId: String,
    createdAt: ZonedDateTime,
    homepageId: String,
    status: String
)

object Space {
  implicit val schema: Schema[MultiEntityResult[Space]] = DeriveSchema.gen
  implicit val codec: BinaryCodec[MultiEntityResult[Space]] =
    JsonCodec.schemaBasedBinaryCodec
}
