package ph.samson.atbp.c2md

object Format {
  def withFrontMatter(title: String, body: String): String = {
    val frontMatter = s"---\ntitle: $title\n---\n"
    val trimmed = body.stripTrailing()
    if (trimmed.isEmpty) {
      frontMatter
    } else {
      s"$frontMatter\n$trimmed"
    }
  }
}
