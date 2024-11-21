package ph.samson.atbp.confluence.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

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

object User {
  implicit val schema: Schema[User] = DeriveSchema.gen
  implicit val codec: BinaryCodec[User] = JsonCodec.schemaBasedBinaryCodec
}
