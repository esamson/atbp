package ph.samson.atbp.md2c

import better.files.File
import zio.ZIO
import zio.test.*

object GitTopologySpec extends ZIOSpecDefault {
  import GitSpecSupport.*

  private val FirstHistoryRecord =
    "2026-10-17\u0000Alex Cruz\u00000123456\u0000Clarify setup\u0000"
  private val SecondHistoryRecord =
    "2026-10-18\u0000Zoë\u0000abcdef0\u0000\u0000"

  private def historyRunner(
      root: File,
      output: String
  ): GitFakes.ScriptedRunner =
    new GitFakes.ScriptedRunner(root) {
      override def run(directory: File, arguments: List[String]) =
        super.run(directory, arguments).map { result =>
          if (GitFakes.key(arguments) == "log")
            GitMetadata.CommandResult(0, output)
          else result
        }
    }

  override def spec = suite("GitTopology")(
    suite("repository topologies")(
      test("a linked worktree resolves its own root, path, and history") {
        for {
          base <- GitFixtures.temp
          main = base / "main"
          linked = base / "linked"
          _ <- ZIO.attemptBlocking {
            GitFixtures.history(main)
            GitFixtures.run(main, "worktree", "add", "-q", linked.toString)
            GitFixtures.commitFile(
              linked,
              "docs/guide.md",
              "four\n",
              "Linked only",
              "2026-01-05T10:00:00Z"
            )
          }
          fromLinked <- GitMetadata.markdown(linked / "docs" / "guide.md")
          fromMain <- GitMetadata.markdown(main / "docs" / "guide.md")
          linkedDir <- GitMetadata.directory(linked / "docs")
          linkedRoot <- GitMetadata.directory(linked)
          linkedHash <- ZIO.attemptBlocking(
            GitFixtures.git(linked, "rev-parse", "--short", "HEAD").trim
          )
          linkedFooter = SourceFooter.markdown(fromLinked)
          expectedFooter = SourceFooter.markdown(
            GitMetadata.MarkdownInfo(
              GitMetadata.DirectoryInfo(
                None,
                Some("docs/guide.md"),
                Some("Clone command unavailable: no Git remote configured.")
              ),
              GitMetadata.HistoryEntry(
                "2026-01-05",
                "Fixture Author",
                linkedHash,
                "Linked only"
              ) :: fromMain.history
            )
          )
        } yield assertTrue(
          (linked / ".git").isRegularFile,
          fromLinked.directory.relativePath.contains("docs/guide.md"),
          fromLinked.history.map(_.subject) ==
            List("Linked only", "Third", "Second", "First"),
          fromLinked.changeStatus == GitMetadata.ChangeStatus.Clean,
          fromLinked.directory.gitNotice.isEmpty,
          linkedFooter.adf.toString == expectedFooter.adf.toString,
          linkedFooter.hashInput == expectedFooter.hashInput,
          // the main checkout does not see the linked worktree's branch
          fromMain.history.map(_.subject) == List("Third", "Second", "First"),
          linkedDir.relativePath.contains("docs"),
          linkedRoot.relativePath.contains("")
        )
      },
      test("a linked worktree reports its own uncommitted state") {
        for {
          base <- GitFixtures.temp
          main = base / "main"
          linked = base / "linked"
          _ <- ZIO.attemptBlocking {
            GitFixtures.history(main)
            GitFixtures.run(main, "worktree", "add", "-q", linked.toString)
            (linked / "docs" / "guide.md").appendLine("local edit")
          }
          dirty <- GitMetadata.markdown(linked / "docs" / "guide.md")
          clean <- GitMetadata.markdown(main / "docs" / "guide.md")
        } yield assertTrue(
          dirty.changeStatus == GitMetadata.ChangeStatus.Uncommitted,
          clean.changeStatus == GitMetadata.ChangeStatus.Clean
        )
      },
      test("a source below a nested publication root uses repository paths") {
        for {
          dir <- GitFixtures.temp
          source <- ZIO.attemptBlocking {
            GitFixtures.init(dir)
            GitFixtures.commitFile(
              dir,
              "docs/a b/-note.md",
              "x\n",
              "Nested",
              "2026-01-01T10:00:00Z"
            )
          }
          info <- GitMetadata.markdown(source)
          directory <- GitMetadata.directory(source.parent)
        } yield assertTrue(
          info.directory.relativePath.contains("docs/a b/-note.md"),
          info.history.map(_.subject) == List("Nested"),
          directory.relativePath.contains("docs/a b")
        )
      },
      test("a shallow clone shows available history in order plus a notice") {
        for {
          base <- GitFixtures.temp
          origin = base / "origin"
          shallow = base / "shallow"
          full = base / "full"
          before <- ZIO.attemptBlocking {
            GitFixtures.history(origin)
            GitFixtures.clone(origin, shallow, "--depth", "3")
            GitFixtures.clone(origin, full)
            GitFixtures.git(shallow, "rev-list", "--count", "HEAD")
          }
          shallowInfo <- GitMetadata.markdown(shallow / "docs" / "guide.md")
          fullInfo <- GitMetadata.markdown(full / "docs" / "guide.md")
          after <- ZIO.attemptBlocking(
            GitFixtures.git(shallow, "rev-list", "--count", "HEAD")
          )
        } yield assertTrue(
          shallowInfo.shallow,
          shallowInfo.history.map(_.subject) == List("Third", "Second"),
          shallowInfo.historyNotice.isEmpty,
          !fullInfo.shallow,
          fullInfo.history.map(_.subject) == List("Third", "Second", "First"),
          // metadata gathering never fetches more history
          before == after
        )
      },
      test("a partial clone with a missing rename blob stays local") {
        for {
          base <- GitFixtures.temp
          origin = base / "origin"
          partial = base / "partial"
          trace = base / "metadata-trace.json"
          setup <- ZIO.attemptBlocking {
            GitFixtures.init(origin)
            GitFixtures.run(origin, "config", "uploadpack.allowFilter", "true")
            GitFixtures.run(
              origin,
              "config",
              "uploadpack.allowAnySHA1InWant",
              "true"
            )
            val lines = (1 to 100).map(n => s"Source paragraph $n")
            GitFixtures.commitFile(
              origin,
              "old.md",
              lines.mkString("", "\n", "\n"),
              "Add source",
              "2026-01-01T10:00:00Z"
            ): Unit
            val oldBlob =
              GitFixtures.git(origin, "rev-parse", "HEAD:old.md").trim
            GitFixtures.run(origin, "mv", "old.md", "guide.md")
            GitFixtures.commitFile(
              origin,
              "guide.md",
              lines.updated(54, "Edited paragraph").mkString("", "\n", "\n"),
              "Rename and edit source",
              "2026-01-02T10:00:00Z"
            ): Unit
            GitFixtures.clone(origin, partial, "--filter=blob:none")
            // The stored URL is a network form; insteadOf keeps fetches local.
            GitFixtures.run(
              partial,
              "remote",
              "set-url",
              "origin",
              "https://example.test/origin.git"
            )
            GitFixtures.run(
              partial,
              "config",
              s"url.${origin.toString}.insteadOf",
              "https://example.test/origin.git"
            )
            val script = executable(
              base / "trace-git",
              """#!/bin/sh
                |export GIT_TRACE2_EVENT="${0%/*}/metadata-trace.json"
                |exec git "$@"
                |""".stripMargin
            )
            (oldBlob, script)
          }
          (oldBlob, script) = setup
          missing <- ZIO.attemptBlocking {
            GitFixtures.git(
              partial,
              "rev-list",
              "--objects",
              "--all",
              "--missing=print"
            )
          }
          storeBefore <- ZIO.attemptBlocking(
            GitFixtures.git(partial, "count-objects", "-v")
          )
          info <- GitMetadata.markdown(
            partial / "guide.md",
            processRunner(script.path.toString)
          )
          storeAfter <- ZIO.attemptBlocking(
            GitFixtures.git(partial, "count-objects", "-v")
          )
          events <- ZIO.attemptBlocking(trace.contentAsString)
          fetchStarted = events.linesIterator
            .filter(_.contains("\"event\":\"child_start\""))
            .exists(_.contains("\"fetch\""))
          // Versions without lazy-fetch suppression can only attempt a denied transport.
          transportDenied = events.contains("transport 'file' not allowed")
          footer = SourceFooter.markdown(info).adf.plainText
        } yield assertTrue(
          missing.linesIterator.contains(s"?$oldBlob"),
          storeBefore == storeAfter,
          events.contains("\"log\",\"--no-show-signature\",\"--follow\""),
          !fetchStarted || transportDenied,
          !events.contains("\"child_class\":\"transport/"),
          info.changeStatus == GitMetadata.ChangeStatus.Clean,
          footer.contains("Source history unavailable."),
          footer.contains("Source: guide.md"),
          footer.contains("git clone -- 'https://example.test/origin.git'")
        )
      },
      test("an untracked source in a shallow clone has no shallow claim") {
        for {
          base <- GitFixtures.temp
          origin = base / "origin"
          shallow = base / "shallow"
          source <- ZIO.attemptBlocking {
            GitFixtures.history(origin)
            GitFixtures.clone(origin, shallow, "--depth", "1")
            val file = shallow / "docs" / "new.md"
            file.write("new\n")
            file
          }
          info <- GitMetadata.markdown(source)
        } yield assertTrue(
          info.history.isEmpty,
          !info.shallow,
          info.changeStatus == GitMetadata.ChangeStatus.Uncommitted
        )
      },
      test("a failed shallow query keeps history and makes no claim") {
        withSource { (root, source) =>
          val runner = new GitFakes.ScriptedRunner(
            root,
            Map("shallow" -> new GitMetadata.TimedOut)
          )
          GitMetadata.markdown(source, runner).map { info =>
            assertTrue(info.history.nonEmpty, !info.shallow)
          }
        }
      },
      test("malformed log records are unavailable, never published") {
        withSource { (root, source) =>
          val outputs = List(
            "signature before record" -> s"No signature\n${FirstHistoryRecord}\n",
            "non-hex hash" -> FirstHistoryRecord
              .replace("0123456", "not-hex")
              .concat("\n"),
            "invalid date" -> FirstHistoryRecord
              .replace("2026-10-17", "10/17/2026")
              .concat("\n"),
            "trailing text" -> s"${FirstHistoryRecord}\nextra text",
            "incomplete second record" -> s"${FirstHistoryRecord}\n2026-10-18\u0000Zoë",
            "missing separator" -> s"${FirstHistoryRecord}${SecondHistoryRecord}\n",
            "invalid separator" -> s"${FirstHistoryRecord}\r\n${SecondHistoryRecord}\n",
            "missing final NUL" -> (FirstHistoryRecord.dropRight(1) + "\n"),
            "missing final newline" -> FirstHistoryRecord,
            "invalid final newline" -> (FirstHistoryRecord + "\r\n")
          )
          ZIO
            .foreach(outputs) { case (label, output) =>
              val runner = historyRunner(root, output)
              GitMetadata.markdown(source, runner).map { info =>
                val footer = SourceFooter.markdown(info).adf.plainText
                assertTrue(
                  info.history.isEmpty,
                  info.historyNotice.contains("Source history unavailable."),
                  info.changeStatus == GitMetadata.ChangeStatus.Clean,
                  footer.contains("Source history unavailable."),
                  footer.contains("Source: guide.md"),
                  footer.contains(s"git clone -- '${GitFakes.RemoteUrl}'"),
                  !footer.contains("Clarify setup"),
                  runner.calls.exists(call =>
                    call.headOption.contains("log") &&
                      call.contains("--no-show-signature")
                  )
                ).label(label)
              }
            }
            .map(_.reduce(_ && _))
        }
      },
      test(
        "empty and complete log output keep their existing history behavior"
      ) {
        withSource { (root, source) =>
          val first = GitMetadata.HistoryEntry(
            "2026-10-17",
            "Alex Cruz",
            "0123456",
            "Clarify setup"
          )
          val second = GitMetadata.HistoryEntry(
            "2026-10-18",
            "Zoë",
            "abcdef0",
            "(no subject)"
          )
          for {
            empty <- GitMetadata.markdown(source, historyRunner(root, ""))
            one <- GitMetadata.markdown(
              source,
              historyRunner(root, FirstHistoryRecord + "\n")
            )
            two <- GitMetadata.markdown(
              source,
              historyRunner(
                root,
                s"${FirstHistoryRecord}\n${SecondHistoryRecord}\n"
              )
            )
          } yield assertTrue(
            empty.history.isEmpty,
            empty.historyNotice.isEmpty,
            SourceFooter
              .markdown(empty)
              .adf
              .plainText
              .contains("No committed history for this source file."),
            one.history == List(first),
            one.historyNotice.isEmpty,
            two.history == List(first, second),
            two.historyNotice.isEmpty
          )
        }
      }
    )
  ) @@ GitFixtures.isolated
}
