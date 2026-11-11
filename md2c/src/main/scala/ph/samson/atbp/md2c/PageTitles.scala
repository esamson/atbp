package ph.samson.atbp.md2c

import better.files.File

object PageTitles {
  val ZeroWidthSpace = "\u200B"

  final case class SourcePage(
      source: File,
      relativePath: String,
      logicalTitle: String
  )

  def assign(pages: List[SourcePage]): Map[File, String] =
    pages.groupBy(_.logicalTitle).flatMap { (title, group) =>
      if (group.sizeIs <= 1) {
        group.map(page => page.source -> title)
      } else {
        group.sortBy(_.relativePath).zipWithIndex.map { (page, index) =>
          page.source -> (title + (ZeroWidthSpace * index))
        }
      }
    }
}
