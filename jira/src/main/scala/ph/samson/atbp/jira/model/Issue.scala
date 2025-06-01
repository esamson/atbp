package ph.samson.atbp.jira.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

import java.time.LocalDate
import java.time.ZonedDateTime

import Issue.*

case class Issue(
    id: String,
    key: String,
    self: String,
    fields: Fields
)

object Issue {
  val FieldNames = List(
    "summary",
    "statuscategorychangedate",
    "created",
    "updated",
    "duedate",
    "resolution",
    "versions",
    "fixVersions",
    "labels",
    "priority",
    "issuetype",
    "status",
    "parent"
  )

  case class Fields(
      summary: String,
      statuscategorychangedate: ZonedDateTime,
      created: ZonedDateTime,
      updated: ZonedDateTime,
      duedate: Option[LocalDate],
      resolution: Option[Resolution],
      versions: List[FixVersion],
      fixVersions: List[FixVersion],
      labels: List[String],
      priority: Option[Priority],
      issuetype: IssueType,
      status: Status,
      parent: Option[Parent]
  )

  case class Resolution(
      id: String,
      name: String,
      description: String
  )

  case class FixVersion(
      id: String,
      name: String,
      description: Option[String],
      releaseDate: Option[LocalDate]
  )

  case class Priority(
      id: Int,
      name: String
  )

  case class IssueType(
      self: String,
      id: String,
      name: String,
      hierarchyLevel: Int
  )

  case class Status(
      id: String,
      name: String,
      description: String,
      statusCategory: Status.Category
  )

  object Status {
    case class Category(
        id: Int,
        key: String,
        name: String
    )
  }

  case class Parent(
      id: String,
      key: String,
      self: String,
      fields: Parent.Fields
  )

  object Parent {
    case class Fields(
        summary: String,
        status: Status,
        priority: Option[Priority],
        issuetype: IssueType
    )
  }

  import Schemas.*
  implicit val schema: Schema[Issue] = DeriveSchema.gen
  implicit val codec: BinaryCodec[Issue] = JsonCodec.schemaBasedBinaryCodec
}
