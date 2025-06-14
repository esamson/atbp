package ph.samson.atbp.jira.model

import ph.samson.atbp.jira.Conf.CustomFields
import zio.Task
import zio.ZIO
import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.Schema.CaseClass14
import zio.schema.Schema.CaseClass4
import zio.schema.Schema.Field
import zio.schema.codec.BinaryCodec
import zio.schema.codec.JsonCodec
import zio.schema.validation.Validation

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
  def fieldNames(customFields: CustomFields) = List(
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
    "parent",
    customFields.startDate
  )

  case class Fields(
      summary: String,
      statuscategorychangedate: ZonedDateTime,
      created: ZonedDateTime,
      updated: ZonedDateTime,
      startDate: Option[LocalDate],
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

  def schema(customFields: CustomFields): Task[Schema[Issue]] = {
    import Schemas.*

    val customFieldsSchema: Task[Schema[Fields]] = {
      val fieldsSchema: Schema[Fields] = DeriveSchema.gen
      fieldsSchema match {
        case CaseClass14(
              id,
              summaryField,
              statuscategorychangedateField,
              createdField,
              updatedField,
              startDateField,
              duedateField,
              resolutionField,
              versionsField,
              fixVersionsField,
              labelsField,
              priorityField,
              issuetypeField,
              statusField,
              parentField,
              construct,
              annotations
            ) =>
          val customStartDateField = startDateField match {
            case Field(name, schema, annotations, validation, get, set) =>
              Field(
                customFields.startDate,
                schema,
                annotations,
                validation,
                get,
                set
              )
          }
          ZIO.succeed(
            CaseClass14(
              id,
              summaryField,
              statuscategorychangedateField,
              createdField,
              updatedField,
              customStartDateField,
              duedateField,
              resolutionField,
              versionsField,
              fixVersionsField,
              labelsField,
              priorityField,
              issuetypeField,
              statusField,
              parentField,
              construct,
              annotations
            )
          )
        case other =>
          ZIO.fail(
            OutdatedSchema(
              s"expected Issue.Fields to be CaseClass14 but got $other"
            )
          )
      }
    }

    for {
      given Schema[Fields] <- customFieldsSchema
    } yield DeriveSchema.gen
  }

  def codec(customFields: CustomFields): Task[BinaryCodec[Issue]] = {
    for {
      given Schema[Issue] <- schema(customFields)
    } yield JsonCodec.schemaBasedBinaryCodec
  }
}
