package ph.samson.atbp.confluence.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

case class Content(
    id: String,
    `type`: String,
    status: String,
    title: String
)

object Content {
  implicit val schema: Schema[MultiEntityResult[Content]] = DeriveSchema.gen
  implicit val codec: BinaryCodec[MultiEntityResult[Content]] =
    JsonCodec.schemaBasedBinaryCodec
}
