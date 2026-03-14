package ph.samson.atbp.confluence.model

import MultiEntityLinks.*

case class MultiEntityLinks(
    base: String,
    next: Option[String]
) {

  def nextUrl: Option[String] =
    next.map(n => mergeUrlParts(base, n))
}

object MultiEntityLinks {
  private def mergeUrlParts(base: String, next: String): String = {
    // Avoid double slashes
    val normalizedFirst =
      if (
        base.nonEmpty
        && next.nonEmpty
        && base.endsWith("/")
        && next.startsWith("/")
      ) {
        base.dropRight(1)
      } else {
        base
      }

    val maxOverlap = math.min(normalizedFirst.length, next.length)

    val overlapLength =
      (maxOverlap to 1 by -1)
        .find(i => normalizedFirst.endsWith(next.take(i)))
        .getOrElse(0)

    if (overlapLength > 0) {
      normalizedFirst + next.drop(overlapLength)
    } else if (normalizedFirst.endsWith("/") || next.startsWith("/")) {
      normalizedFirst + next
    } else {
      // Ensure that the second part starts with a slash
      s"$normalizedFirst/$next"
    }
  }
}
