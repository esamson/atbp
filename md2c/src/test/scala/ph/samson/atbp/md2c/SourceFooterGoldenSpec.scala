package ph.samson.atbp.md2c

import zio.test.*

object SourceFooterGoldenSpec extends ZIOSpecDefault {
  import GitMetadata.*

  private val clone = "git clone -- 'git@example.com:team/a.git'"
  private val entry =
    HistoryEntry("2026-10-17", "Alex Cruz", "0123456", "Clarify *setup*")
  private val entry2 = HistoryEntry("2026-10-18", "Bo | Lee", "89abcde", "日本語")
  private val dir = DirectoryInfo(Some(clone), Some("docs/guide.md"))

  private[md2c] val fixtures: List[(String, SourceFooter.Rendered)] = List(
    "md-clean" -> SourceFooter.markdown(MarkdownInfo(dir, List(entry, entry2))),
    "md-uncommitted" -> SourceFooter.markdown(
      MarkdownInfo(dir, List(entry), changeStatus = ChangeStatus.Uncommitted)
    ),
    "md-status-unavailable" -> SourceFooter.markdown(
      MarkdownInfo(dir, List(entry), changeStatus = ChangeStatus.Unavailable)
    ),
    "md-no-history" -> SourceFooter.markdown(MarkdownInfo(dir, Nil)),
    "md-history-unavailable" -> SourceFooter.markdown(
      MarkdownInfo(
        dir,
        Nil,
        historyNotice = Some("Source history unavailable.")
      )
    ),
    "md-path-unavailable" -> SourceFooter.markdown(
      MarkdownInfo(DirectoryInfo(Some(clone), None), List(entry))
    ),
    "md-remote-unavailable-notice" -> SourceFooter.markdown(
      MarkdownInfo(
        DirectoryInfo(
          None,
          Some("docs/guide.md"),
          cloneNotice =
            Some("Clone command unavailable: no Git remote configured.")
        ),
        List(entry)
      )
    ),
    "md-remote-lookup-failed" -> SourceFooter.markdown(
      MarkdownInfo(DirectoryInfo(None, Some("docs/guide.md")), List(entry))
    ),
    "md-shallow" -> SourceFooter.markdown(
      MarkdownInfo(dir, List(entry), shallow = true)
    ),
    "md-git-unavailable" -> SourceFooter.markdown(
      MarkdownInfo(
        DirectoryInfo(
          None,
          None,
          gitNotice = Some("Git metadata unavailable.")
        ),
        Nil,
        changeStatus = ChangeStatus.Unavailable
      )
    ),
    "dir-clean" -> SourceFooter.directory(
      DirectoryInfo(Some(clone), Some("docs/a b"))
    ),
    "dir-root" -> SourceFooter.directory(DirectoryInfo(Some(clone), Some(""))),
    "dir-path-unavailable" -> SourceFooter.directory(
      DirectoryInfo(Some(clone), None)
    ),
    "dir-remote-notice" -> SourceFooter.directory(
      DirectoryInfo(
        None,
        Some("docs"),
        cloneNotice =
          Some("Clone command unavailable: no Git remote configured.")
      )
    ),
    "dir-remote-lookup-failed" -> SourceFooter.directory(
      DirectoryInfo(None, Some("docs"))
    ),
    "dir-git-unavailable" -> SourceFooter.directory(
      DirectoryInfo(None, None, gitNotice = Some("Git metadata unavailable."))
    )
  )

  case class Golden(
      name: String,
      hashInput: String,
      adfJson: String,
      finalHash: String
  )

  val goldens: List[Golden] = List(
    Golden(
      "md-clean",
      """21:md2c-source-footer-v163:Do not edit here. This page was generated from markdown source.41:git clone -- 'git@example.com:team/a.git'7:Source:13:docs/guide.md40:Recent source history (up to 5 commits):10:2026-10-179:Alex Cruz7:012345615:Clarify *setup*10:2026-10-188:Bo | Lee7:89abcde9:日本語""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page was generated from markdown source."}]},{"type":"codeBlock","content":[{"type":"text","text":"git clone -- 'git@example.com:team/a.git'"}]},{"type":"paragraph","content":[{"type":"text","text":"Source: "},{"type":"text","text":"docs/guide.md","marks":[{"type":"code"}]}]},{"type":"paragraph","content":[{"type":"text","text":"Recent source history (up to 5 commits):"}]},{"type":"paragraph","content":[{"type":"text","text":"2026-10-17 | Alex Cruz | "},{"type":"text","text":"0123456","marks":[{"type":"code"}]},{"type":"text","text":" | Clarify *setup*"}]},{"type":"paragraph","content":[{"type":"text","text":"2026-10-18 | Bo | Lee | "},{"type":"text","text":"89abcde","marks":[{"type":"code"}]},{"type":"text","text":" | 日本語"}]}]}""",
      "z9o_qk_wfOCeg6zKSzQp97dP_8U="
    ),
    Golden(
      "md-uncommitted",
      """21:md2c-source-footer-v163:Do not edit here. This page was generated from markdown source.41:git clone -- 'git@example.com:team/a.git'7:Source:13:docs/guide.md78:Source has uncommitted changes; history below contains committed changes only.40:Recent source history (up to 5 commits):10:2026-10-179:Alex Cruz7:012345615:Clarify *setup*""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page was generated from markdown source."}]},{"type":"codeBlock","content":[{"type":"text","text":"git clone -- 'git@example.com:team/a.git'"}]},{"type":"paragraph","content":[{"type":"text","text":"Source: "},{"type":"text","text":"docs/guide.md","marks":[{"type":"code"}]}]},{"type":"paragraph","content":[{"type":"text","text":"Source has uncommitted changes; history below contains committed changes only."}]},{"type":"paragraph","content":[{"type":"text","text":"Recent source history (up to 5 commits):"}]},{"type":"paragraph","content":[{"type":"text","text":"2026-10-17 | Alex Cruz | "},{"type":"text","text":"0123456","marks":[{"type":"code"}]},{"type":"text","text":" | Clarify *setup*"}]}]}""",
      "fxn4DHjQVgHFvs0KrFSIwWElBgw="
    ),
    Golden(
      "md-status-unavailable",
      """21:md2c-source-footer-v163:Do not edit here. This page was generated from markdown source.41:git clone -- 'git@example.com:team/a.git'7:Source:13:docs/guide.md33:Source change status unavailable.40:Recent source history (up to 5 commits):10:2026-10-179:Alex Cruz7:012345615:Clarify *setup*""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page was generated from markdown source."}]},{"type":"codeBlock","content":[{"type":"text","text":"git clone -- 'git@example.com:team/a.git'"}]},{"type":"paragraph","content":[{"type":"text","text":"Source: "},{"type":"text","text":"docs/guide.md","marks":[{"type":"code"}]}]},{"type":"paragraph","content":[{"type":"text","text":"Source change status unavailable."}]},{"type":"paragraph","content":[{"type":"text","text":"Recent source history (up to 5 commits):"}]},{"type":"paragraph","content":[{"type":"text","text":"2026-10-17 | Alex Cruz | "},{"type":"text","text":"0123456","marks":[{"type":"code"}]},{"type":"text","text":" | Clarify *setup*"}]}]}""",
      "QSOOmpWO6pEni4kM_DgsThjWpTs="
    ),
    Golden(
      "md-no-history",
      """21:md2c-source-footer-v163:Do not edit here. This page was generated from markdown source.41:git clone -- 'git@example.com:team/a.git'7:Source:13:docs/guide.md42:No committed history for this source file.""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page was generated from markdown source."}]},{"type":"codeBlock","content":[{"type":"text","text":"git clone -- 'git@example.com:team/a.git'"}]},{"type":"paragraph","content":[{"type":"text","text":"Source: "},{"type":"text","text":"docs/guide.md","marks":[{"type":"code"}]}]},{"type":"paragraph","content":[{"type":"text","text":"No committed history for this source file."}]}]}""",
      "oULvVdTmyy9daXu4YFjnYYaPmfM="
    ),
    Golden(
      "md-history-unavailable",
      """21:md2c-source-footer-v163:Do not edit here. This page was generated from markdown source.41:git clone -- 'git@example.com:team/a.git'7:Source:13:docs/guide.md27:Source history unavailable.""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page was generated from markdown source."}]},{"type":"codeBlock","content":[{"type":"text","text":"git clone -- 'git@example.com:team/a.git'"}]},{"type":"paragraph","content":[{"type":"text","text":"Source: "},{"type":"text","text":"docs/guide.md","marks":[{"type":"code"}]}]},{"type":"paragraph","content":[{"type":"text","text":"Source history unavailable."}]}]}""",
      "QyNXcPsCVc9P5DPgwFHL_P7yr-Y="
    ),
    Golden(
      "md-path-unavailable",
      """21:md2c-source-footer-v163:Do not edit here. This page was generated from markdown source.41:git clone -- 'git@example.com:team/a.git'7:Source:44:Repository-relative source path unavailable.40:Recent source history (up to 5 commits):10:2026-10-179:Alex Cruz7:012345615:Clarify *setup*""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page was generated from markdown source."}]},{"type":"codeBlock","content":[{"type":"text","text":"git clone -- 'git@example.com:team/a.git'"}]},{"type":"paragraph","content":[{"type":"text","text":"Source: "},{"type":"text","text":"Repository-relative source path unavailable.","marks":[{"type":"code"}]}]},{"type":"paragraph","content":[{"type":"text","text":"Recent source history (up to 5 commits):"}]},{"type":"paragraph","content":[{"type":"text","text":"2026-10-17 | Alex Cruz | "},{"type":"text","text":"0123456","marks":[{"type":"code"}]},{"type":"text","text":" | Clarify *setup*"}]}]}""",
      "oHAjAGRse7K54do0Nxh_axFLtpA="
    ),
    Golden(
      "md-remote-unavailable-notice",
      """21:md2c-source-footer-v163:Do not edit here. This page was generated from markdown source.52:Clone command unavailable: no Git remote configured.7:Source:13:docs/guide.md40:Recent source history (up to 5 commits):10:2026-10-179:Alex Cruz7:012345615:Clarify *setup*""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page was generated from markdown source."}]},{"type":"paragraph","content":[{"type":"text","text":"Clone command unavailable: no Git remote configured."}]},{"type":"paragraph","content":[{"type":"text","text":"Source: "},{"type":"text","text":"docs/guide.md","marks":[{"type":"code"}]}]},{"type":"paragraph","content":[{"type":"text","text":"Recent source history (up to 5 commits):"}]},{"type":"paragraph","content":[{"type":"text","text":"2026-10-17 | Alex Cruz | "},{"type":"text","text":"0123456","marks":[{"type":"code"}]},{"type":"text","text":" | Clarify *setup*"}]}]}""",
      "EnJpcacQfR4pb1RH55EkphxcRo8="
    ),
    Golden(
      "md-remote-lookup-failed",
      """21:md2c-source-footer-v163:Do not edit here. This page was generated from markdown source.48:Clone command unavailable: remote lookup failed.7:Source:13:docs/guide.md40:Recent source history (up to 5 commits):10:2026-10-179:Alex Cruz7:012345615:Clarify *setup*""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page was generated from markdown source."}]},{"type":"paragraph","content":[{"type":"text","text":"Clone command unavailable: remote lookup failed."}]},{"type":"paragraph","content":[{"type":"text","text":"Source: "},{"type":"text","text":"docs/guide.md","marks":[{"type":"code"}]}]},{"type":"paragraph","content":[{"type":"text","text":"Recent source history (up to 5 commits):"}]},{"type":"paragraph","content":[{"type":"text","text":"2026-10-17 | Alex Cruz | "},{"type":"text","text":"0123456","marks":[{"type":"code"}]},{"type":"text","text":" | Clarify *setup*"}]}]}""",
      "e6kEjB0YTOwoFQ5XvSO0YKYUJDg="
    ),
    Golden(
      "md-shallow",
      """21:md2c-source-footer-v163:Do not edit here. This page was generated from markdown source.41:git clone -- 'git@example.com:team/a.git'7:Source:13:docs/guide.md40:Recent source history (up to 5 commits):10:2026-10-179:Alex Cruz7:012345615:Clarify *setup*44:History may be incomplete: shallow checkout.""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page was generated from markdown source."}]},{"type":"codeBlock","content":[{"type":"text","text":"git clone -- 'git@example.com:team/a.git'"}]},{"type":"paragraph","content":[{"type":"text","text":"Source: "},{"type":"text","text":"docs/guide.md","marks":[{"type":"code"}]}]},{"type":"paragraph","content":[{"type":"text","text":"Recent source history (up to 5 commits):"}]},{"type":"paragraph","content":[{"type":"text","text":"2026-10-17 | Alex Cruz | "},{"type":"text","text":"0123456","marks":[{"type":"code"}]},{"type":"text","text":" | Clarify *setup*"}]},{"type":"paragraph","content":[{"type":"text","text":"History may be incomplete: shallow checkout."}]}]}""",
      "1aDolM9HPaOoyofnU1AaKnCZANY="
    ),
    Golden(
      "md-git-unavailable",
      """21:md2c-source-footer-v163:Do not edit here. This page was generated from markdown source.25:Git metadata unavailable.""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page was generated from markdown source."}]},{"type":"paragraph","content":[{"type":"text","text":"Git metadata unavailable."}]}]}""",
      "XIOQ4k3W1V8MoQZ0RlHMLynhMsg="
    ),
    Golden(
      "dir-clean",
      """21:md2c-source-footer-v161:Do not edit here. This page is a generated directory listing.41:git clone -- 'git@example.com:team/a.git'17:Source directory:8:docs/a b""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page is a generated directory listing."}]},{"type":"codeBlock","content":[{"type":"text","text":"git clone -- 'git@example.com:team/a.git'"}]},{"type":"paragraph","content":[{"type":"text","text":"Source directory: "},{"type":"text","text":"docs/a b","marks":[{"type":"code"}]}]}]}""",
      "t61-CXjgtD8E0n_tUdXrbGYu3kk="
    ),
    Golden(
      "dir-root",
      """21:md2c-source-footer-v161:Do not edit here. This page is a generated directory listing.41:git clone -- 'git@example.com:team/a.git'17:Source directory:1:.""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page is a generated directory listing."}]},{"type":"codeBlock","content":[{"type":"text","text":"git clone -- 'git@example.com:team/a.git'"}]},{"type":"paragraph","content":[{"type":"text","text":"Source directory: "},{"type":"text","text":".","marks":[{"type":"code"}]}]}]}""",
      "WYtOIFTI-gYrkIzGWLd-mYBz3Do="
    ),
    Golden(
      "dir-path-unavailable",
      """21:md2c-source-footer-v161:Do not edit here. This page is a generated directory listing.41:git clone -- 'git@example.com:team/a.git'17:Source directory:44:Repository-relative source path unavailable.""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page is a generated directory listing."}]},{"type":"codeBlock","content":[{"type":"text","text":"git clone -- 'git@example.com:team/a.git'"}]},{"type":"paragraph","content":[{"type":"text","text":"Source directory: "},{"type":"text","text":"Repository-relative source path unavailable.","marks":[{"type":"code"}]}]}]}""",
      "aAbrjSWgIghg-HvQhnQB-Z7U6vU="
    ),
    Golden(
      "dir-remote-notice",
      """21:md2c-source-footer-v161:Do not edit here. This page is a generated directory listing.52:Clone command unavailable: no Git remote configured.17:Source directory:4:docs""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page is a generated directory listing."}]},{"type":"paragraph","content":[{"type":"text","text":"Clone command unavailable: no Git remote configured."}]},{"type":"paragraph","content":[{"type":"text","text":"Source directory: "},{"type":"text","text":"docs","marks":[{"type":"code"}]}]}]}""",
      "Ead5N_Gx9zblnzEyA0UQDgiOJKM="
    ),
    Golden(
      "dir-remote-lookup-failed",
      """21:md2c-source-footer-v161:Do not edit here. This page is a generated directory listing.48:Clone command unavailable: remote lookup failed.17:Source directory:4:docs""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page is a generated directory listing."}]},{"type":"paragraph","content":[{"type":"text","text":"Clone command unavailable: remote lookup failed."}]},{"type":"paragraph","content":[{"type":"text","text":"Source directory: "},{"type":"text","text":"docs","marks":[{"type":"code"}]}]}]}""",
      "iiVKaiNmUu9iL5yDRqU14dJhNk8="
    ),
    Golden(
      "dir-git-unavailable",
      """21:md2c-source-footer-v161:Do not edit here. This page is a generated directory listing.25:Git metadata unavailable.""",
      """{"type":"doc","version":1,"content":[{"type":"rule"},{"type":"paragraph","content":[{"type":"text","text":"Do not edit here. This page is a generated directory listing."}]},{"type":"paragraph","content":[{"type":"text","text":"Git metadata unavailable."}]}]}""",
      "1NuYzhkljvRQIMjzZUDAY9mvbII="
    )
  )

  private val adfParser = new com.atlassian.adf.jackson2.AdfJackson2()

  override def spec = suite("SourceFooter golden")(
    test("every fixture has a golden and vice versa") {
      assertTrue(fixtures.map(_._1) == goldens.map(_.name))
    },
    suite("pinned output")(
      fixtures.zip(goldens).map { case ((name, rendered), golden) =>
        test(name) {
          for {
            finalHash <- Parser.computeHash(
              SourceFooter.frame(List("content-hash-fixed", rendered.hashInput))
            )
          } yield assertTrue(
            rendered.hashInput == golden.hashInput,
            adfParser.marshall(rendered.adf) == golden.adfJson,
            finalHash == golden.finalHash
          )
        }
      }*
    )
  )
}
