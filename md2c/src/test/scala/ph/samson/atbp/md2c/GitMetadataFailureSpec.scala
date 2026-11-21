package ph.samson.atbp.md2c

import better.files.File
import zio.Chunk
import zio.LogLevel
import zio.Ref
import zio.ZIO
import zio.test.*

object GitMetadataFailureSpec extends ZIOSpecDefault {
  import GitSpecSupport.*

  private def indexTimeout(calls: Ref[List[String]]): GitMetadata.Runner =
    new GitMetadata.Runner {
      override def run(
          directory: File,
          arguments: List[String]
      ): zio.Task[GitMetadata.CommandResult] =
        calls.update(arguments.head :: _) *> {
          if (arguments.head == "ls-files")
            ZIO.fail(new GitMetadata.TimedOut)
          else GitFixtures.runner.run(directory, arguments)
        }
    }

  override def spec = suite("GitMetadata failures")(
    suite("independent field failures")(
      test("a failed remote list keeps path, status, and history") {
        withSource { (root, source) =>
          val runner = new GitFakes.ScriptedRunner(
            root,
            Map("remote" -> new GitMetadata.TimedOut)
          )
          GitMetadata.markdown(source, runner).map { info =>
            assertTrue(
              info.directory.cloneCommand.isEmpty,
              info.directory.cloneNotice.contains(
                "Clone command unavailable: remote lookup failed."
              ),
              info.directory.relativePath.contains("guide.md"),
              info.changeStatus == GitMetadata.ChangeStatus.Clean,
              info.history.map(_.hash) == List("0123456"),
              info.historyNotice.isEmpty
            )
          }
        }
      },
      test("a nonzero remote list is a lookup failure, not an empty list") {
        withSource { (root, source) =>
          val runner =
            new GitFakes.ScriptedRunner(root, exitCodes = Map("remote" -> 128))
          GitMetadata.directory(source, runner).map { info =>
            assertTrue(
              info.cloneNotice.contains(
                "Clone command unavailable: remote lookup failed."
              ),
              info.relativePath.contains("guide.md")
            )
          }
        }
      },
      test(
        "an unresolvable source path logs a fixed warning and keeps the footer"
      ) {
        withSource { (root, _) =>
          val dangling = root / "dangling-secret.md"
          val runner = new GitFakes.ScriptedRunner(root)
          for {
            _ <- ZIO.attemptBlocking(
              java.nio.file.Files.createSymbolicLink(
                dangling.path,
                root.path.resolve("missing-target-secret.md")
              )
            )
            info <- GitMetadata.directory(dangling, runner)
            logs <- ZTestLogger.logOutput
            warnings = logs.filter(_.logLevel == LogLevel.Warning)
            rendered = logs.map(entry => s"${entry.message()} ${entry.cause}")
          } yield assertTrue(
            info.relativePath.isEmpty,
            warnings.map(_.message()) == Chunk(
              "Git metadata path unavailable: source path could not be relativized."
            ),
            rendered.forall(!_.contains("secret")),
            rendered.forall(!_.contains(root.path.toString))
          )
        }
      },
      test("a failed URL lookup keeps path, status, and history") {
        withSource { (root, source) =>
          val runner = new GitFakes.ScriptedRunner(
            root,
            Map("config" -> new GitMetadata.Failed("io"))
          )
          GitMetadata.markdown(source, runner).map { info =>
            assertTrue(
              info.directory.cloneNotice.contains(
                "Clone command unavailable: remote lookup failed."
              ),
              info.directory.relativePath.contains("guide.md"),
              info.history.nonEmpty
            )
          }
        }
      },
      test("failed status checks keep remote and history") {
        withSource { (root, source) =>
          val runner = new GitFakes.ScriptedRunner(
            root,
            Map("ls-files" -> new GitMetadata.TimedOut)
          )
          GitMetadata.markdown(source, runner).map { info =>
            assertTrue(
              info.changeStatus == GitMetadata.ChangeStatus.Unavailable,
              info.directory.cloneCommand.nonEmpty,
              info.history.nonEmpty,
              info.historyNotice.isEmpty
            )
          }
        }
      },
      test(
        "an ls-files failure keeps history verified by one regular HEAD file"
      ) {
        withSource { (root, source) =>
          val absent = new GitFakes.ScriptedRunner(
            root,
            Map("ls-files" -> new GitMetadata.TimedOut),
            exitCodes = Map("ls-tree" -> 0)
          )
          val lookupFailed = new GitFakes.ScriptedRunner(
            root,
            Map(
              "ls-files" -> new GitMetadata.TimedOut,
              "ls-tree" -> new GitMetadata.TimedOut
            )
          )
          val present = new GitFakes.ScriptedRunner(
            root,
            Map("ls-files" -> new GitMetadata.TimedOut),
            lsTreeOutput = Some(s"100755 blob ${"a" * 64}\tguide.md\u0000")
          )
          for {
            missing <- GitMetadata.markdown(source, absent)
            failed <- GitMetadata.markdown(source, lookupFailed)
            kept <- GitMetadata.markdown(source, present)
          } yield assertTrue(
            missing.history.isEmpty,
            missing.historyNotice.contains("Source history unavailable."),
            missing.changeStatus == GitMetadata.ChangeStatus.Unavailable,
            !absent.calls.exists(_.headOption.contains("log")),
            failed.history.isEmpty,
            failed.historyNotice.contains("Source history unavailable."),
            !lookupFailed.calls.exists(_.headOption.contains("log")),
            kept.history.nonEmpty,
            kept.historyNotice.isEmpty,
            kept.changeStatus == GitMetadata.ChangeStatus.Unavailable,
            present.calls.contains(
              List("ls-tree", "-z", "HEAD", "--", "guide.md")
            )
          )
        }
      },
      test(
        "an invalid HEAD entry stops the fallback before the history query"
      ) {
        withSource { (root, source) =>
          val blob = s"100644 blob ${GitFakes.ObjectId}"
          val entry = s"$blob\tguide.md\u0000"
          val invalid = List(
            "directory" -> s"040000 tree ${GitFakes.ObjectId}\tguide.md\u0000",
            "symlink" -> s"120000 blob ${GitFakes.ObjectId}\tguide.md\u0000",
            "submodule" -> s"160000 commit ${GitFakes.ObjectId}\tguide.md\u0000",
            "wrong type" -> s"100644 tree ${GitFakes.ObjectId}\tguide.md\u0000",
            "invalid object ID" -> "100644 blob invalid\tguide.md\u0000",
            "missing tab" -> s"$blob guide.md\u0000",
            "missing terminator" -> s"$blob\tguide.md",
            "different filename" -> s"$blob\tother.md\u0000",
            "multiple entries" -> (entry + entry)
          )
          ZIO
            .foreach(invalid) { case (description, output) =>
              val runner = new GitFakes.ScriptedRunner(
                root,
                Map("ls-files" -> new GitMetadata.TimedOut),
                lsTreeOutput = Some(output)
              )
              GitMetadata.markdown(source, runner).map { info =>
                assertTrue(
                  info.history.isEmpty,
                  info.historyNotice.contains("Source history unavailable."),
                  info.changeStatus == GitMetadata.ChangeStatus.Unavailable,
                  info.directory.cloneCommand.nonEmpty,
                  info.directory.relativePath.contains("guide.md"),
                  !runner.calls.exists(_.headOption.contains("log"))
                ).label(description)
              }
            }
            .map(_.reduce(_ && _))
        }
      },
      test(
        "a path deleted from HEAD shows no old history on an index failure"
      ) {
        for {
          dir <- GitFixtures.temp
          source <- ZIO.attemptBlocking {
            GitFixtures.init(dir)
            val _ = GitFixtures.commitFile(
              dir,
              "guide.md",
              "old\n",
              "Old guide",
              "2026-01-01T10:00:00Z"
            )
            GitFixtures.run(dir, "rm", "-q", "guide.md")
            GitFixtures.commitAll(dir, "Remove guide", "2026-01-02T10:00:00Z")
            val untracked = dir / "guide.md"
            untracked.write("new\n")
            untracked
          }
          calls <- Ref.make(List.empty[String])
          info <- GitMetadata.markdown(source, indexTimeout(calls))
          commands <- calls.get
        } yield assertTrue(
          info.history.isEmpty,
          info.historyNotice.contains("Source history unavailable."),
          info.changeStatus == GitMetadata.ChangeStatus.Unavailable,
          !commands.contains("log")
        )
      },
      test(
        "a directory in HEAD cannot supply old file history on an index timeout"
      ) {
        for {
          dir <- GitFixtures.temp
          source <- ZIO.attemptBlocking {
            GitFixtures.init(dir)
            GitFixtures.run(dir, "remote", "add", "origin", GitFakes.RemoteUrl)
            val old = GitFixtures.commitFile(
              dir,
              "guide.md",
              "old\n",
              "Old guide",
              "2026-01-01T10:00:00Z"
            )
            old.delete()
            val child = GitFixtures.commitFile(
              dir,
              "guide.md/chapter.md",
              "chapter\n",
              "Replace guide with directory",
              "2026-01-02T10:00:00Z"
            )
            child.parent.delete()
            old.write("new\n")
          }
          calls <- Ref.make(List.empty[String])
          info <- GitMetadata.markdown(source, indexTimeout(calls))
          commands <- calls.get
        } yield assertTrue(
          info.history.isEmpty,
          info.historyNotice.contains("Source history unavailable."),
          info.changeStatus == GitMetadata.ChangeStatus.Unavailable,
          info.directory.cloneCommand
            .contains(s"git clone -- '${GitFakes.RemoteUrl}'"),
          info.directory.relativePath.contains("guide.md"),
          commands.count(_ == "ls-tree") == 1,
          !commands.contains("log")
        )
      },
      test(
        "HEAD lookup preserves tabs and pathspec metacharacters in filenames"
      ) {
        val path = "guide\t[*]?.md"
        for {
          dir <- GitFixtures.temp
          source <- ZIO.attemptBlocking {
            GitFixtures.init(dir)
            GitFixtures.commitFile(
              dir,
              path,
              "guide\n",
              "Literal guide",
              "2026-01-01T10:00:00Z"
            )
          }
          calls <- Ref.make(List.empty[String])
          info <- GitMetadata.markdown(source, indexTimeout(calls))
          commands <- calls.get
        } yield assertTrue(
          info.history.map(_.subject) == List("Literal guide"),
          info.historyNotice.isEmpty,
          info.changeStatus == GitMetadata.ChangeStatus.Unavailable,
          info.directory.relativePath.contains(path),
          commands.count(_ == "ls-tree") == 1,
          commands.count(_ == "log") == 1
        )
      },
      test("diff exit 0 is clean, 1 is changed, anything else unavailable") {
        withSource { (root, source) =>
          val failing = new GitFakes.ScriptedRunner(
            root,
            Map("diff" -> new GitMetadata.TimedOut)
          )
          val changed = new GitFakes.ScriptedRunner(
            root,
            exitCodes = Map("diff" -> 1)
          )
          val oddExit = new GitFakes.ScriptedRunner(
            root,
            exitCodes = Map("diff" -> 129)
          )
          val healthy = new GitFakes.ScriptedRunner(root)
          for {
            unknown <- GitMetadata.markdown(source, failing)
            dirty <- GitMetadata.markdown(source, changed)
            odd <- GitMetadata.markdown(source, oddExit)
            clean <- GitMetadata.markdown(source, healthy)
            cleanCommands = healthy.calls.map(GitFakes.key)
          } yield assertTrue(
            unknown.changeStatus == GitMetadata.ChangeStatus.Unavailable,
            dirty.changeStatus == GitMetadata.ChangeStatus.Uncommitted,
            odd.changeStatus == GitMetadata.ChangeStatus.Unavailable,
            clean.changeStatus == GitMetadata.ChangeStatus.Clean,
            clean.history.map(_.hash) == List("0123456"),
            cleanCommands.size == 8,
            cleanCommands.count(_ == "log") == 1,
            !cleanCommands.contains("ls-tree"),
            !failing.calls.exists(_.headOption.contains("diff-index"))
          )
        }
      },
      test("a failed history query keeps remote and status") {
        withSource { (root, source) =>
          val runner = new GitFakes.ScriptedRunner(
            root,
            Map("log" -> new GitMetadata.TimedOut)
          )
          GitMetadata.markdown(source, runner).map { info =>
            assertTrue(
              info.history.isEmpty,
              info.historyNotice.contains("Source history unavailable."),
              info.directory.cloneCommand.nonEmpty,
              info.changeStatus == GitMetadata.ChangeStatus.Clean
            )
          }
        }
      },
      test("repository lookup failures choose the specified global notice") {
        withSource { (root, source) =>
          def notice(runner: GitMetadata.Runner) =
            GitMetadata.directory(source, runner).map(_.gitNotice)
          for {
            missing <- notice(
              new GitFakes.ScriptedRunner(
                root,
                Map("toplevel" -> new GitMetadata.ExecutableUnavailable)
              )
            )
            timeout <- notice(
              new GitFakes.ScriptedRunner(
                root,
                Map("toplevel" -> new GitMetadata.TimedOut)
              )
            )
            defect <- notice(
              new GitFakes.ScriptedRunner(
                root,
                Map("toplevel" -> new java.io.IOException("boom"))
              )
            )
            unknown <- notice(
              new GitFakes.ScriptedRunner(
                root,
                exitCodes = Map("toplevel" -> 128)
              )
            )
          } yield assertTrue(
            missing.contains(
              "Git metadata unavailable: git executable not available."
            ),
            timeout.contains(
              "Git metadata unavailable: repository lookup failed."
            ),
            defect == timeout,
            unknown == timeout
          )
        }
      },
      test("logs contain sanitized categories, never raw failure text") {
        withSource { (root, source) =>
          val secret = "https://user:s3cret-token@example.com/team/docs.git"
          val runner = new GitFakes.ScriptedRunner(
            root,
            Map(
              "remote" -> new RuntimeException(s"fatal: $secret"),
              "log" -> new java.io.IOException(s"cannot read $secret"),
              "ls-files" -> new GitMetadata.TimedOut
            )
          )
          for {
            _ <- GitMetadata.markdown(source, runner)
            logs <- ZTestLogger.logOutput
            rendered = logs.map(entry => s"${entry.message()} ${entry.cause}")
          } yield assertTrue(
            logs.nonEmpty,
            rendered.forall(!_.contains("s3cret")),
            rendered.forall(!_.contains(secret)),
            rendered.exists(_.contains("subcommand=remote reason=error")),
            rendered.exists(_.contains("subcommand=log reason=io-error")),
            rendered.exists(_.contains("subcommand=ls-files reason=timeout")),
            rendered.forall(!_.contains(root.path.toString))
          )
        }
      },
      test("the runner is injectable per fiber for publication paths") {
        withSource { (root, source) =>
          val runner = new GitFakes.ScriptedRunner(root)
          GitMetadata
            .withRunner(runner)(GitMetadata.markdown(source))
            .map(info =>
              assertTrue(
                info.history.nonEmpty,
                runner.calls.nonEmpty
              )
            )
        }
      }
    )
  ) @@ GitFixtures.isolated
}
