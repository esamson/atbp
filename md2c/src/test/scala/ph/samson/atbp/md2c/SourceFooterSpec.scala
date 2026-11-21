package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.model.node.CodeBlock
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Paragraph
import com.atlassian.adf.model.node.Rule
import com.atlassian.adf.model.node.Text
import zio.ZIO
import zio.test.*

object SourceFooterSpec extends ZIOSpecDefault {
  private def withSource[R, E, A](use: (File, File) => ZIO[R, E, A]) =
    ZIO
      .acquireRelease(ZIO.attemptBlocking(File.newTemporaryDirectory()))(dir =>
        ZIO.attemptBlocking(dir.delete(swallowIOExceptions = true)).orDie
      )
      .flatMap { dir =>
        val source = dir / "guide.md"
        ZIO.attemptBlocking(source.write("hello\n")) *> use(dir, source)
      }

  override def spec = suite("SourceFooter")(
    test("directory footer is literal, ordered, and hashes visible fields") {
      val footer = SourceFooter.directory(
        GitMetadata.DirectoryInfo(
          Some("git clone -- 'git@example.com:team/a.git'"),
          Some("docs/a b")
        )
      )
      val changed = SourceFooter.directory(
        GitMetadata.DirectoryInfo(
          Some("git clone -- 'git@example.com:team/b.git'"),
          Some("docs/a b")
        )
      )
      val blocks = footer.adf.content()
      val plain = blocks.toArray.toList.map(_.toString).mkString("\n")
      assertTrue(
        footer.hashInput == SourceFooter
          .directory(
            GitMetadata.DirectoryInfo(
              Some("git clone -- 'git@example.com:team/a.git'"),
              Some("docs/a b")
            )
          )
          .hashInput,
        footer.hashInput != changed.hashInput,
        blocks.size() == 4,
        (blocks.get(2) match { case _: CodeBlock => true; case _ => false }),
        plain.contains(
          "Do not edit here. This page is a generated directory listing."
        ),
        plain.contains("git clone -- 'git@example.com:team/a.git'"),
        plain.contains("docs/a b")
      )
    },
    test("markdown footer includes uncommitted notice before history") {
      val dirty = SourceFooter.markdown(
        GitMetadata.MarkdownInfo(
          GitMetadata.DirectoryInfo(
            Some("git clone -- 'git@example.com:team/a.git'"),
            Some("docs/guide.md")
          ),
          List(
            GitMetadata.HistoryEntry(
              "2026-10-17",
              "Alex Cruz",
              "0123456",
              "Clarify setup"
            )
          ),
          changeStatus = GitMetadata.ChangeStatus.Uncommitted
        )
      )
      val clean = SourceFooter.markdown(
        GitMetadata.MarkdownInfo(
          GitMetadata.DirectoryInfo(
            Some("git clone -- 'git@example.com:team/a.git'"),
            Some("docs/guide.md")
          ),
          List(
            GitMetadata.HistoryEntry(
              "2026-10-17",
              "Alex Cruz",
              "0123456",
              "Clarify setup"
            )
          ),
          changeStatus = GitMetadata.ChangeStatus.Clean
        )
      )
      assertTrue(
        dirty.adf.plainText.contains(
          "Source has uncommitted changes; history below contains committed changes only."
        ),
        !clean.adf.plainText.contains("uncommitted changes"),
        dirty.hashInput != clean.hashInput
      )
    },
    test("markdown footer renders source and committed history literally") {
      val footer = SourceFooter.markdown(
        GitMetadata.MarkdownInfo(
          GitMetadata.DirectoryInfo(
            Some("git clone -- 'git@example.com:team/a.git'"),
            Some("docs/guide.md")
          ),
          List(
            GitMetadata.HistoryEntry(
              "2026-10-17",
              "Alex Cruz",
              "0123456",
              "Clarify *setup* — 日本語"
            )
          )
        )
      )
      val blocks = footer.adf.content().toArray.toList
      val text = footer.adf.plainText
      assertTrue(
        blocks.size == 6,
        (blocks.head match { case _: Rule => true; case _ => false }),
        (blocks(2) match { case _: CodeBlock => true; case _ => false }),
        text.contains(
          "Do not edit here. This page was generated from markdown source."
        ),
        text.contains("Source: docs/guide.md"),
        text.contains("Recent source history (up to 5 commits):"),
        text.contains(
          "2026-10-17 | Alex Cruz | 0123456 | Clarify *setup* — 日本語"
        ),
        footer.hashInput == SourceFooter
          .markdown(
            GitMetadata.MarkdownInfo(
              GitMetadata.DirectoryInfo(
                Some("git clone -- 'git@example.com:team/a.git'"),
                Some("docs/guide.md")
              ),
              List(
                GitMetadata.HistoryEntry(
                  "2026-10-17",
                  "Alex Cruz",
                  "0123456",
                  "Clarify *setup* — 日本語"
                )
              )
            )
          )
          .hashInput,
        footer.hashInput != SourceFooter
          .markdown(
            GitMetadata.MarkdownInfo(
              GitMetadata.DirectoryInfo(
                Some("git clone -- 'git@example.com:team/a.git'"),
                Some("docs/guide.md")
              ),
              Nil
            )
          )
          .hashInput
      )
    },
    test("field framing distinguishes adjacent values") {
      assertTrue(
        SourceFooter.frame(List("ab", "c")) !=
          SourceFooter.frame(List("a", "bc"))
      )
    },
    test("attach appends footer blocks and folds its hash input in") {
      val footer = SourceFooter.directory(
        GitMetadata.DirectoryInfo(Some("git clone -- 'x'"), Some("docs"))
      )
      val doc = Doc.doc(Paragraph.paragraph(Text.text("body")))
      for {
        attached <- SourceFooter.attach(doc, "h1", footer)
        expected <- Parser.computeHash(
          SourceFooter.frame(List("h1", footer.hashInput))
        )
      } yield assertTrue(
        attached._1.content().size() ==
          1 + footer.adf.content().size(),
        attached._2 == expected
      )
    },
    test("remote selection and clone quoting are safe") {
      assertTrue(
        CloneUrl.selectRemote(List("backup", "origin", "mirror")) == Some(
          "origin"
        ),
        CloneUrl.selectRemote(List("backup")) == Some("backup"),
        CloneUrl.selectRemote(Nil).isEmpty,
        CloneUrl.selectRemote(List("one", "two")).isEmpty,
        CloneUrl.safeCloneCommand("git@example.com:team/a b's.git") ==
          Some("git clone -- 'git@example.com:team/a b'\"'\"'s.git'"),
        CloneUrl.safeCloneCommand("-dangerous.git").isEmpty,
        CloneUrl
          .safeCloneCommand("https://user:secret@example.com/repo")
          .isEmpty
      )
    },
    test(
      "unusable fetch URLs never enter either footer, its hash input, or logs"
    ) {
      val notice =
        "Clone command unavailable: selected remote URL contains credentials or is unusable."
      val firstSecret = "fixture-first-value"
      val secondSecret = "fixture-second-value"
      def footers(root: File, source: File, url: String) =
        for {
          _ <- ZIO.attemptBlocking(
            GitFixtures.run(root, "config", "remote.origin.url", url)
          )
          markdown <- GitMetadata.markdown(source)
          directory <- GitMetadata.directory(root)
        } yield (
          SourceFooter.markdown(markdown),
          SourceFooter.directory(directory)
        )

      for {
        root <- GitFixtures.temp
        source <- ZIO.attemptBlocking {
          GitFixtures.init(root)
          GitFixtures.commitFile(
            root,
            "guide.md",
            "hello\n",
            "Add guide",
            "2026-01-01T10:00:00Z"
          )
        }
        first <- footers(
          root,
          source,
          s"https://user:$firstSecret@broken host/docs.git"
        )
        second <- footers(
          root,
          source,
          s"https://example.test/docs.git#${secondSecret}"
        )
        logs <- ZTestLogger.logOutput
        logMessages = logs.map(entry => s"${entry.message()} ${entry.cause}")
        rendered = List(first._1, first._2, second._1, second._2)
      } yield assertTrue(
        rendered.forall(_.adf.plainText.contains(notice)),
        rendered.forall(footer =>
          List(footer.adf.toString, footer.hashInput).forall(value =>
            !value.contains(firstSecret) && !value.contains(secondSecret)
          )
        ),
        logMessages.forall(line =>
          !line.contains(firstSecret) && !line.contains(secondSecret)
        ),
        first._1.adf.plainText.contains("Source: guide.md"),
        first._1.adf.plainText.contains("Add guide"),
        first._2.adf.plainText.contains("Source directory: ."),
        first._1.hashInput == second._1.hashInput,
        first._2.hashInput == second._2.hashInput
      )
    },
    suite("degraded metadata")(
      test("each failed field uses its exact notice and keeps known facts") {
        withSource { (root, source) =>
          val runner = new GitFakes.ScriptedRunner(
            root,
            Map(
              "remote" -> new GitMetadata.TimedOut,
              "ls-files" -> new GitMetadata.TimedOut,
              "log" -> new GitMetadata.TimedOut
            )
          )
          GitMetadata.markdown(source, runner).map { info =>
            val footer = SourceFooter.markdown(info)
            val blocks = footer.adf.content().toArray.toList
            val text = footer.adf.plainText
            assertTrue(
              (blocks.head match { case _: Rule => true; case _ => false }),
              text.contains(
                "Do not edit here. This page was generated from markdown source."
              ),
              text.contains(
                "Clone command unavailable: remote lookup failed."
              ),
              text.contains("Source: guide.md"),
              text.contains("Source change status unavailable."),
              text.contains("Source history unavailable."),
              !text.contains("No committed history")
            )
          }
        }
      },
      test("an unavailable path keeps the clone command and warning") {
        withSource { (_, source) =>
          val runner = new GitFakes.ScriptedRunner(File("/no/such/root"))
          GitMetadata.markdown(source, runner).map { info =>
            val text = SourceFooter.markdown(info).adf.plainText
            assertTrue(
              text.contains(GitFakes.RemoteUrl),
              text.contains("Repository-relative source path unavailable."),
              text.contains(
                "Do not edit here. This page was generated from markdown source."
              )
            )
          }
        }
      },
      test("a global Git notice replaces field notices but keeps the warning") {
        withSource { (root, source) =>
          val runner = new GitFakes.ScriptedRunner(
            root,
            Map("toplevel" -> new GitMetadata.ExecutableUnavailable)
          )
          GitMetadata.markdown(source, runner).map { info =>
            val footer = SourceFooter.markdown(info)
            val text = footer.adf.plainText
            assertTrue(
              footer.adf.content().size() == 3,
              text.contains(
                "Do not edit here. This page was generated from markdown source."
              ),
              text.contains(
                "Git metadata unavailable: git executable not available."
              ),
              !text.contains("Source:"),
              !text.contains("Clone command unavailable"),
              !text.contains("Source history unavailable.")
            )
          }
        }
      },
      test("directory pages keep the warning and path through remote failure") {
        withSource { (root, _) =>
          val runner = new GitFakes.ScriptedRunner(
            root,
            Map("remote" -> new GitMetadata.TimedOut)
          )
          GitMetadata.directory(root, runner).map { info =>
            val text = SourceFooter.directory(info).adf.plainText
            assertTrue(
              text.contains(
                "Do not edit here. This page is a generated directory listing."
              ),
              text.contains(
                "Clone command unavailable: remote lookup failed."
              ),
              text.contains("Source directory: ."),
              !text.contains("history")
            )
          }
        }
      },
      test("failure causes never reach the rendered footer or hash input") {
        withSource { (root, source) =>
          val secret = "https://user:s3cret-token@example.com/team/docs.git"
          def footer(error: Throwable) =
            GitMetadata
              .markdown(
                source,
                new GitFakes.ScriptedRunner(
                  root,
                  Map(
                    "remote" -> error,
                    "ls-files" -> error,
                    "log" -> error
                  )
                )
              )
              .map(SourceFooter.markdown)
          for {
            timeout <- footer(new GitMetadata.TimedOut)
            detailed <- footer(
              new RuntimeException(s"fatal: unable to access '$secret'")
            )
            io <- footer(new java.io.IOException(secret))
          } yield assertTrue(
            timeout.hashInput == detailed.hashInput,
            timeout.hashInput == io.hashInput,
            timeout.adf.plainText == detailed.adf.plainText,
            !detailed.hashInput.contains("s3cret"),
            !detailed.adf.plainText.contains("s3cret"),
            !io.adf.plainText.contains("s3cret")
          )
        }
      },
      test("degraded and recovered footers hash differently") {
        withSource { (root, source) =>
          val failing = new GitFakes.ScriptedRunner(
            root,
            Map("remote" -> new GitMetadata.TimedOut)
          )
          val healthy = new GitFakes.ScriptedRunner(root)
          for {
            degraded <- GitMetadata
              .markdown(source, failing)
              .map(SourceFooter.markdown)
            repeat <- GitMetadata
              .markdown(source, failing)
              .map(SourceFooter.markdown)
            recovered <- GitMetadata
              .markdown(source, healthy)
              .map(SourceFooter.markdown)
          } yield assertTrue(
            degraded.hashInput == repeat.hashInput,
            degraded.hashInput != recovered.hashInput
          )
        }
      }
    ),
    suite("repository variants")(
      test("shallow notice follows history and changes the hash") {
        val info = GitMetadata.MarkdownInfo(
          GitMetadata.DirectoryInfo(
            Some("git clone -- 'git@example.com:team/a.git'"),
            Some("docs/guide.md")
          ),
          List(
            GitMetadata.HistoryEntry("2026-01-03", "A", "abc1234", "Third"),
            GitMetadata.HistoryEntry("2026-01-02", "A", "def5678", "Second")
          )
        )
        val shallow = SourceFooter.markdown(info.copy(shallow = true))
        val full = SourceFooter.markdown(info)
        val texts = shallow.adf.content().toArray.toList.map(_.toString)
        assertTrue(
          shallow.adf.plainText.contains(
            "History may be incomplete: shallow checkout."
          ),
          shallow.adf.plainText.indexOf("Second") <
            shallow.adf.plainText.indexOf("shallow checkout"),
          texts.last.contains("History may be incomplete: shallow checkout."),
          !full.adf.plainText.contains("shallow"),
          shallow.hashInput != full.hashInput,
          shallow.hashInput == SourceFooter
            .markdown(info.copy(shallow = true))
            .hashInput
        )
      },
      test("shallow flag without displayed history is not rendered") {
        val info = GitMetadata.MarkdownInfo(
          GitMetadata.DirectoryInfo(None, Some("docs/new.md")),
          Nil,
          shallow = true
        )
        val footer = SourceFooter.markdown(info)
        assertTrue(
          !footer.adf.plainText.contains("shallow"),
          footer.hashInput == SourceFooter
            .markdown(info.copy(shallow = false))
            .hashInput
        )
      },
      test("a root directory listing shows a dot and never a shallow note") {
        val footer = SourceFooter.directory(
          GitMetadata.DirectoryInfo(
            Some("git clone -- 'git@example.com:team/a.git'"),
            Some("")
          )
        )
        assertTrue(
          footer.adf.plainText.contains("Source directory: ."),
          !footer.adf.plainText.contains("shallow")
        )
      }
    ),
    suite("visible lines and hash input")(
      test("any change to the visible footer changes the hash input") {
        import GitMetadata.*
        val parser = new com.atlassian.adf.jackson2.AdfJackson2()
        val clone = "git clone -- 'git@example.com:team/a.git'"
        val entry = HistoryEntry("2026-10-17", "Alex", "0123456", "Subject")
        val dir = DirectoryInfo(Some(clone), Some("docs/guide.md"))
        val mdBase = MarkdownInfo(dir, List(entry))
        val markdowns: List[MarkdownInfo] = List(
          mdBase,
          mdBase.copy(directory = dir.copy(cloneCommand = Some("git clone x"))),
          mdBase.copy(directory =
            dir.copy(relativePath = Some("docs/other.md"))
          ),
          mdBase.copy(directory = dir.copy(relativePath = Some(""))),
          mdBase.copy(directory = dir.copy(relativePath = None)),
          mdBase.copy(directory =
            dir.copy(cloneCommand = None, cloneNotice = Some("No remote."))
          ),
          mdBase.copy(directory =
            dir.copy(cloneCommand = None, cloneNotice = Some("Other notice."))
          ),
          mdBase.copy(directory = dir.copy(cloneCommand = None)),
          mdBase.copy(directory =
            dir.copy(gitNotice = Some("Git unavailable."))
          ),
          mdBase.copy(directory = dir.copy(gitNotice = Some("Git broken."))),
          mdBase.copy(changeStatus = ChangeStatus.Uncommitted),
          mdBase.copy(changeStatus = ChangeStatus.Unavailable),
          mdBase.copy(shallow = true),
          mdBase.copy(history = Nil),
          mdBase.copy(history = List(entry.copy(date = "2026-10-18"))),
          mdBase.copy(history = List(entry.copy(author = "Bo"))),
          mdBase.copy(history = List(entry.copy(hash = "89abcde"))),
          mdBase.copy(history = List(entry.copy(subject = "Other"))),
          mdBase.copy(history = List(entry, entry)),
          mdBase.copy(historyNotice = Some("History unavailable.")),
          mdBase.copy(historyNotice = Some("History broken.")),
          mdBase.copy(history = Nil, historyNotice = Some("History broken."))
        )
        val dirBase = DirectoryInfo(Some(clone), Some("docs"))
        val directories: List[DirectoryInfo] = List(
          dirBase,
          dirBase.copy(cloneCommand = Some("git clone x")),
          dirBase.copy(relativePath = Some("other")),
          dirBase.copy(relativePath = Some("")),
          dirBase.copy(relativePath = None),
          dirBase.copy(cloneCommand = None),
          dirBase.copy(cloneCommand = None, cloneNotice = Some("No remote.")),
          dirBase.copy(gitNotice = Some("Git unavailable.")),
          dirBase.copy(gitNotice = Some("Git broken."))
        )
        def footers(variants: List[SourceFooter.Rendered]) =
          variants.map(r => (parser.marshall(r.adf), r.hashInput))
        def check(all: List[(String, String)]) =
          for {
            a <- all
            b <- all
            if a._1 != b._1
          } yield a._2 != b._2
        val md = footers(markdowns.map(SourceFooter.markdown))
        val dr = footers(directories.map(SourceFooter.directory))
        val mdChecks = check(md)
        val drChecks = check(dr)
        assertTrue(
          md.map(_._1).distinct.size > 15,
          dr.map(_._1).distinct.size > 6,
          mdChecks.nonEmpty,
          drChecks.nonEmpty,
          mdChecks.forall(identity),
          drChecks.forall(identity)
        )
      }
    )
  )
}
