package ph.samson.atbp.jira.model

import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec

import java.time.LocalDate
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try

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

  private val JiraDateTime =
    DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  implicit val jdtSchema: Schema[ZonedDateTime] =
    Schema[String].transformOrFail(
      string =>
        Try(
          ZonedDateTime.parse(string, JiraDateTime)
        ).toEither.left.map(_.toString),
      zonedDateTime =>
        Try(zonedDateTime.format(JiraDateTime)).toEither.left.map(_.toString)
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
        priority: Priority,
        issuetype: IssueType
    )
  }

  implicit val schema: Schema[Issue] = DeriveSchema.gen
  implicit val codec: BinaryCodec[Issue] = JsonCodec.schemaBasedBinaryCodec
}
