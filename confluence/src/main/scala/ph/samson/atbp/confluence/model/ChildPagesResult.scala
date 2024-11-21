package ph.samson.atbp.confluence.model

import ph.samson.atbp.confluence.model.ChildPagesResult.*
import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

case class ChildPagesResult(
    results: List[ChildPage],
    _links: MultiEntityLinks
)

object ChildPagesResult {
  case class ChildPage(
      id: String,
      status: String,
      title: String,
      spaceId: String,
      childPosition: Int
  )

  implicit val schema: Schema[ChildPagesResult] = DeriveSchema.gen
  implicit val codec: BinaryCodec[ChildPagesResult] =
    JsonCodec.schemaBasedBinaryCodec
}
