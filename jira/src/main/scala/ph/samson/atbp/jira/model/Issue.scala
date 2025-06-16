package ph.samson.atbp.jira.model

import ph.samson.atbp.jira.Conf.CustomFields
import zio.Task
import zio.ZIO
import zio.schema.DeriveSchema
import zio.schema.Schema
import zio.schema.Schema.CaseClass15
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
    customFields.sprints,
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
      parent: Option[Parent],
      sprints: Option[List[Sprint]]
  ) {
    def getSprints: List[Sprint] = sprints match {
      case Some(list) => list
      case None       => Nil
    }
  }

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

  /** A Sprint.
    *
    * On the Jira Software Cloud API, there is a "sprint" field, which is a
    * single Sprint object.
    *
    * On the Jira Cloud platform REST API, sprint is a custom field, which is an
    * array of similar but not exactly the same Sprint objects.
    *
    * {{{
    * {
    *     "id": "1381729",
    *     "self": "https://xxxx.atlassian.net/rest/agile/1.0/issue/1081120",
    *     "key": "XXX-318",
    *     "fields": {
    *         "customfield_10736": [
    *             {
    *                 "id": 19718,
    *                 "name": "my-sprint",
    *                 "state": "active",
    *                 "boardId": 2398,
    *                 "goal": "",
    *                 "startDate": "2025-06-11T09:18:26.610Z",
    *                 "endDate": "2025-06-24T12:00:00.000Z"
    *             }
    *         ],
    *         "sprint": {
    *             "id": 11721,
    *             "self": "https://xxxx.atlassian.net/rest/agile/1.0/sprint/19718",
    *             "state": "active",
    *             "name": "my-sprint",
    *             "startDate": "2025-06-11T09:18:26.610Z",
    *             "endDate": "2025-06-24T12:00:00.000Z",
    *             "createdDate": "2025-06-11T08:10:00.645Z",
    *             "originBoardId": 2398,
    *             "goal": ""
    *         }
    *     }
    * }
    * }}}
    *
    * We use the platform version as least common denominator.
    */
  case class Sprint(
      id: Int,
      name: String,
      state: String,
      boardId: Int,
      goal: String,
      startDate: ZonedDateTime,
      endDate: ZonedDateTime
  )

  def schema(customFields: CustomFields): Task[Schema[Issue]] = {
    import Schemas.*

    val customFieldsSchema: Task[Schema[Fields]] = {
      val fieldsSchema: Schema[Fields] = DeriveSchema.gen
      fieldsSchema match {
        case CaseClass15(
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
              sprintsField,
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
          val customSprintsField = sprintsField match {
            case Field(name, schema, annotations, validation, get, set) =>
              Field(
                customFields.sprints,
                schema,
                annotations,
                validation,
                get,
                set
              )
          }
          ZIO.succeed(
            CaseClass15(
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
              customSprintsField,
              construct,
              annotations
            )
          )
        case other =>
          ZIO.fail(
            OutdatedSchema(
              s"expected Issue.Fields to be CaseClass15 but got $other"
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
