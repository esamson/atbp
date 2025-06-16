package ph.samson.atbp.jira

case class Conf(
    site: String,
    user: String,
    token: String,
    customFields: Conf.CustomFields
)

object Conf {
  case class CustomFields(sprints: String, startDate: String)
  object CustomFields {

    /** Default values if no custom fields are given.
      *
      * This will result in these fields always being empty in results.
      */
    val Default: CustomFields = CustomFields("sprintField", "startDateField")
  }
}
