package ph.samson.atbp.jira.model

import java.time.ZonedDateTime

case class Comment(
    self: String,
    id: String,
    author: UserDetails,
    updateAuthor: UserDetails,
    created: ZonedDateTime,
    updated: ZonedDateTime,
    jsdPublic: Boolean
)
