package ph.samson.atbp.jira.model

import zio.schema.Schema

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try

object Schemas {

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

}
