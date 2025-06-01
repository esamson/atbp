package ph.samson.atbp.jira.model

case class UserDetails(
    self: String,
    accountId: String,
    emailAddress: Option[String],
    displayName: String,
    active: Boolean,
    timeZone: String,
    accountType: String,
    key: Option[String],
    name: Option[String]
)
