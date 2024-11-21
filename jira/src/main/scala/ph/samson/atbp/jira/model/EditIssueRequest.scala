package ph.samson.atbp.jira.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

import EditIssueRequest.*

case class EditIssueRequest(
    update: Map[String, List[FieldUpdateOperation]] = Map.empty
)

object EditIssueRequest {
  case class FieldUpdateOperation(
      add: Option[String] = None,
      remove: Option[String] = None,
      set: Option[String] = None
  )

  object FieldUpdateOperation {
    def add(value: String) = FieldUpdateOperation(add = Some(value))
    def remove(value: String) = FieldUpdateOperation(remove = Some(value))
  }

  def addLabel(value: String) = EditIssueRequest(
    update = Map("labels" -> List(FieldUpdateOperation.add(value)))
  )

  def removeLabel(value: String) = EditIssueRequest(
    update = Map("labels" -> List(FieldUpdateOperation.remove(value)))
  )

  implicit val schema: Schema[EditIssueRequest] = DeriveSchema.gen
  implicit val codec: BinaryCodec[EditIssueRequest] =
    JsonCodec.schemaBasedBinaryCodec
}
