package ph.samson.atbp.jira.model

case class PageBean[T](
    self: String,
    nextPage: Option[String],
    maxResults: Int,
    startAt: Int,
    total: Int,
    isLast: Boolean,
    values: List[T]
)
