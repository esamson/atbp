package ph.samson.atbp.confluence.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

case class CreatePageRequest(
    spaceId: String,
    status: String,
    title: String,
    parentId: String,
    body: PageBodyWrite
)

object CreatePageRequest {
  implicit val schema: Schema[CreatePageRequest] = DeriveSchema.gen
  implicit val codec: BinaryCodec[CreatePageRequest] =
    JsonCodec.schemaBasedBinaryCodec
}
