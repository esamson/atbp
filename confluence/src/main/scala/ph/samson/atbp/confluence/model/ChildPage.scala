package ph.samson.atbp.confluence.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

case class ChildPage(
    id: String,
    status: String,
    title: String,
    spaceId: String,
    childPosition: Int
)

object ChildPage {

  implicit val schema: Schema[MultiEntityResult[ChildPage]] = DeriveSchema.gen
  implicit val codec: BinaryCodec[MultiEntityResult[ChildPage]] =
    JsonCodec.schemaBasedBinaryCodec
}
