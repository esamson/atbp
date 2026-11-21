package ph.samson.atbp.md2c

import better.files.File
import zio.ZIO
import zio.test.*

import java.time.Instant

object GitMetadataSpec extends ZIOSpecDefault {
  import GitSpecSupport.*

  private class FakeRunner(
      results: Map[List[String], GitMetadata.CommandResult]
  ) extends GitMetadata.Runner {
    private val captured =
      new java.util.concurrent.atomic.AtomicReference(List.empty[List[String]])
    def calls: List[List[String]] = captured.get()
    override def run(
        directory: File,
        arguments: List[String]
    ) = ZIO.succeed {
      captured.updateAndGet(_ :+ arguments)
      results.getOrElse(arguments, GitMetadata.CommandResult(1, ""))
    }
  }

  override def spec = suite("GitMetadata")(
    test(
      "directory lookup uses origin fetch URL and never asks file history or status"
    ) {
      val root = File(System.getProperty("user.dir"))
      val runner = new FakeRunner(
        Map(
          List("rev-parse", "--show-toplevel") ->
            GitMetadata.CommandResult(0, root.path.toString),
          List("remote") -> GitMetadata.CommandResult(0, "backup\norigin\n"),
          List("config", "--null", "--get-all", "remote.origin.url") ->
            GitMetadata.CommandResult(
              0,
              "git@example.com:team/handbook.git\u0000https://ignored.example/second.git\u0000"
            )
        )
      )
      for {
        info <- GitMetadata.directory(root, runner)
      } yield assertTrue(
        info.cloneCommand.contains(
          "git clone -- 'git@example.com:team/handbook.git'"
        ),
        info.relativePath.contains(""),
        runner.calls.map(_.take(1)) == List(
          List("rev-parse"),
          List("remote"),
          List("config")
        ),
        !runner.calls.exists(args =>
          args.contains("log") || args.contains("status")
        )
      )
    },
    test("no remote and ambiguous remote selection are explicit") {
      assertTrue(
        CloneUrl.selectRemote(Nil).isEmpty,
        CloneUrl.selectRemote(List("one", "two")).isEmpty
      )
    },
    test("a tracked page looks up its worktree root and HEAD once") {
      withSource { (root, source) =>
        val runner = new GitFakes.ScriptedRunner(root)
        val unindexedRunner = new GitFakes.ScriptedRunner(
          root,
          exitCodes = Map("ls-files" -> 1)
        )
        for {
          tracked <- GitMetadata.markdown(source, runner)
          unindexed <- GitMetadata.markdown(source, unindexedRunner)
          trackedCalls = runner.calls.map(GitFakes.key)
          unindexedCalls = unindexedRunner.calls.map(GitFakes.key)
        } yield assertTrue(
          trackedCalls.count(_ == "toplevel") == 1,
          trackedCalls.count(_ == "head") == 1,
          tracked.changeStatus == GitMetadata.ChangeStatus.Clean,
          tracked.history.map(_.subject) == List("Clarify setup"),
          unindexedCalls.count(_ == "toplevel") == 1,
          !unindexedCalls.contains("head"),
          !unindexedCalls.contains("log"),
          unindexed.changeStatus == GitMetadata.ChangeStatus.Uncommitted,
          unindexed.history.isEmpty
        )
      }
    },
    test("the first configured fetch URL is read as one complete value") {
      for {
        root <- GitFixtures.temp
        _ <- ZIO.attemptBlocking {
          GitFixtures.init(root)
          GitFixtures.run(
            root,
            "remote",
            "add",
            "backup",
            "git@backup.test:docs"
          )
          GitFixtures.run(
            root,
            "remote",
            "add",
            "origin",
            "git@first.test:docs"
          )
          GitFixtures.run(
            root,
            "config",
            "--add",
            "remote.origin.url",
            "git@second.test:docs"
          )
          GitFixtures.run(
            root,
            "config",
            "remote.origin.pushurl",
            "git@push.test:docs"
          )
        }
        first <- GitMetadata.directory(root)
        _ <- ZIO.attemptBlocking {
          GitFixtures.run(root, "config", "--unset-all", "remote.origin.url")
          GitFixtures.run(root, "config", "--add", "remote.origin.url", "")
          GitFixtures.run(
            root,
            "config",
            "--add",
            "remote.origin.url",
            "git@second.test:docs"
          )
        }
        empty <- GitMetadata.directory(root)
        _ <- ZIO.attemptBlocking {
          GitFixtures.run(root, "config", "--unset-all", "remote.origin.url")
          GitFixtures.run(
            root,
            "config",
            "--add",
            "remote.origin.url",
            "git@first.test:docs\nsynthetic-private-value"
          )
          GitFixtures.run(
            root,
            "config",
            "--add",
            "remote.origin.url",
            "git@second.test:docs"
          )
        }
        multiline <- GitMetadata.directory(root)
      } yield assertTrue(
        first.cloneCommand.contains("git clone -- 'git@first.test:docs'"),
        first.cloneNotice.isEmpty,
        empty.cloneCommand.isEmpty,
        empty.cloneNotice.exists(_.endsWith("credentials or is unusable.")),
        multiline.cloneCommand.isEmpty,
        multiline.cloneNotice == empty.cloneNotice
      )
    },
    test("a relative origin URL is unusable in a real repository") {
      for {
        root <- GitFixtures.temp
        _ <- ZIO.attemptBlocking {
          GitFixtures.init(root)
          GitFixtures.run(root, "remote", "add", "origin", "../shared/docs.git")
        }
        info <- GitMetadata.directory(root)
      } yield assertTrue(
        info.cloneCommand.isEmpty,
        info.cloneNotice.contains(
          "Clone command unavailable: selected remote URL contains credentials or is unusable."
        )
      )
    },
    test("a repository-configured fsmonitor command is never run") {
      for {
        root <- GitFixtures.temp
        marker = root / "fsmonitor-ran"
        source <- ZIO.attemptBlocking {
          GitFixtures.init(root)
          val source = GitFixtures.commitFile(
            root,
            "docs/guide.md",
            "hello\n",
            "Add guide",
            "2024-01-02T03:04:05Z"
          )
          val script = root / "fsmonitor.sh"
          script.write(s"#!/bin/sh\ntouch '${marker.path}'\nexit 1\n")
          script.toJava.setExecutable(true)
          GitFixtures.run(
            root,
            "config",
            "core.fsmonitor",
            script.path.toString
          )
          source.write("changed\n")
          source
        }
        markdown <- GitMetadata.markdown(source)
        afterMarkdown <- ZIO.attemptBlocking {
          val ran = marker.exists
          marker.delete(swallowIOExceptions = true)
          ran
        }
        directory <- GitMetadata.directory(source.parent)
        afterDirectory <- ZIO.attemptBlocking(marker.exists)
      } yield assertTrue(
        markdown.changeStatus == GitMetadata.ChangeStatus.Uncommitted,
        !afterMarkdown,
        directory.cloneCommand.isEmpty,
        !afterDirectory
      )
    },
    test("credential URLs are never rendered") {
      assertTrue(
        CloneUrl
          .safeCloneCommand("https://token@example.com/team/repo.git")
          .isEmpty,
        CloneUrl
          .safeCloneCommand("https://user:secret@example.com/team/repo.git")
          .isEmpty,
        CloneUrl
          .safeCloneCommand(
            "https://example.com/team/repo.git?access_token=secret"
          )
          .isEmpty,
        CloneUrl.safeCloneCommand("git@github.com:team/repo.git").nonEmpty
      )
    },
    test("clone commands accept validated network URLs and SCP syntax") {
      val safe = List(
        "https://example.test/team/docs.git",
        "http://example.test:8080/team/docs.git",
        "git://example.test/team/docs.git",
        "ssh://git@example.test:2222/team/docs.git",
        "git+ssh://git@example.test/team/docs.git",
        "ssh+git://git@example.test/team/docs.git",
        "git@[2001:db8::1]:team/docs.git"
      )
      assertTrue(
        safe.forall(url =>
          CloneUrl.safeCloneCommand(url).contains(s"git clone -- '${url}'")
        ),
        CloneUrl
          .safeCloneCommand("git@example.test:team/a b's.git")
          .contains("git clone -- 'git@example.test:team/a b'\"'\"'s.git'")
      )
    },
    test("clone commands reject malformed, ambiguous, and unsupported forms") {
      val rejected = List(
        "https://user:fixture-secret@broken host/team/docs.git",
        "user:fixture-secret@example.test/team/docs.git",
        "https://user%3Afee@example.test/team/docs.git",
        "ssh://git%3Afixture-secret@example.test/docs.git",
        "https://example.test/team/docs.git?key=fixture-secret",
        "https://example.test/team/docs.git#fixture-secret",
        "https://example.test/team/docs.git%3Fkey=fixture-secret",
        "git@example.test:team/docs.git?key=fixture-secret",
        "https:///team/docs.git",
        "HTTPS:/team/docs.git",
        "https://example.test:bad/team/docs.git",
        "ssh://git@example.test:65536/docs.git",
        "ftp://example.test/team/docs.git",
        "file:///private/fixture/repo.git",
        "/private/fixture/repo.git",
        "C:/private/fixture/repo.git",
        "\\\\server\\repo.git",
        "git@example.test:",
        "helper::repository",
        "../shared/docs.git",
        "-local.git",
        "docs.git",
        "./docs.git"
      )
      assertTrue(rejected.forall(CloneUrl.safeCloneCommand(_).isEmpty))
    },
    test(
      "markdown history follows renames and keeps author metadata and subjects"
    ) {
      GitFixtures.temp
        .flatMap { dir =>
          def git(args: String*): Unit = GitFixtures.run(dir, args*)
          ZIO
            .attemptBlocking {
              git("init", "-q")
              git("config", "user.name", "Stored Author")
              git("config", "user.email", "author@example.test")
              val old = dir / "old.md"
              old.write("shared content\nsecond shared line\n")
              git("add", "old.md")
              git("commit", "-q", "-m", "Older title\n\nbody must not appear")
              val renamed = dir / "renamed.md"
              old.moveTo(renamed)
              renamed.write("shared content\nsecond shared line\nthird line\n")
              git("add", "-A")
              git("commit", "-q", "-m", "日本語 guide")
              (dir / ".mailmap").write(
                "Mapped Name <author@example.test> Stored Author <author@example.test>\n"
              )
              renamed
            }
            .flatMap { source =>
              GitMetadata.markdown(source).map { info =>
                val histories = info.history
                assertTrue(
                  info.directory.relativePath.contains("renamed.md"),
                  histories.map(_.subject) == List("日本語 guide", "Older title"),
                  histories.forall(_.author == "Stored Author"),
                  histories.forall(_.date.matches("\\d{4}-\\d{2}-\\d{2}")),
                  histories.forall(_.hash.matches("[0-9a-f]+")),
                  info.historyNotice.isEmpty
                )
              }
            }
        }
    },
    test(
      "a signed commit gives the same history with log.showSignature enabled"
    ) {
      GitFixtures.temp.flatMap { base =>
        val repo = base / "repo"
        for {
          _ <- ZIO.attemptBlocking {
            GitFixtures.init(repo)
            GitFixtures.commitFile(
              repo,
              "guide.md",
              "one\n",
              "First",
              "2026-01-01T10:00:00Z"
            ): Unit
            GitFixtures.commitFile(
              repo,
              "guide.md",
              "two\n",
              "Second",
              "2026-01-02T10:00:00Z"
            ): Unit
            // Fake SSH signature: no key or ssh-keygen is needed to show the output.
            val commit = GitFixtures.git(repo, "cat-file", "commit", "HEAD")
            val split = commit.indexOf("\n\n")
            val signed =
              commit.substring(0, split) +
                "\ngpgsig -----BEGIN SSH SIGNATURE-----\n" +
                " U1NIU0lHAAAAAQ==\n" +
                " -----END SSH SIGNATURE-----" +
                commit.substring(split)
            val file = base / "signed-commit.txt"
            file.write(signed)
            val hash = GitFixtures
              .git(
                repo,
                "hash-object",
                "-t",
                "commit",
                "-w",
                file.path.toString
              )
              .trim
            GitFixtures.run(repo, "update-ref", "refs/heads/main", hash)
            GitFixtures.run(repo, "config", "gpg.format", "ssh")
          }
          source = repo / "guide.md"
          plain <- GitMetadata.markdown(source)
          _ <- ZIO.attemptBlocking(
            GitFixtures.run(repo, "config", "log.showSignature", "true")
          )
          signed <- GitMetadata.markdown(source)
        } yield assertTrue(
          plain.history.map(_.subject) == List("Second", "First"),
          signed.history == plain.history,
          signed.historyNotice.isEmpty,
          signed.history.forall(_.date.matches("\\d{4}-\\d{2}-\\d{2}")),
          signed.history.forall(_.hash.matches("[0-9a-f]+"))
        )
      }
    },
    test(
      "untracked source is uncommitted and does not inherit pathname history"
    ) {
      GitFixtures.temp
        .flatMap { dir =>
          def git(args: String*): Unit = GitFixtures.run(dir, args*)
          ZIO
            .attemptBlocking {
              git("init", "-q")
              git("config", "user.name", "Author")
              git("config", "user.email", "a@example.test")
              val tracked = dir / "guide.md"
              tracked.write("v1\n")
              git("add", "guide.md")
              git("commit", "-q", "-m", "tracked")
              git("rm", "-q", "guide.md")
              git("commit", "-q", "-m", "remove")
              val replacement = dir / "guide.md"
              replacement.write("untracked\n")
              replacement
            }
            .flatMap { source =>
              GitMetadata.markdown(source).map { info =>
                assertTrue(
                  info.changeStatus == GitMetadata.ChangeStatus.Uncommitted,
                  info.history.isEmpty,
                  info.historyNotice.isEmpty
                )
              }
            }
        }
    },
    test(
      "an unindexed literal filename does not inherit a matching sibling's history"
    ) {
      for {
        root <- GitFixtures.temp
        source <- ZIO.attemptBlocking {
          GitFixtures.init(root)
          GitFixtures.commitFile(
            root,
            "guide1.md",
            "sibling\n",
            "Add sibling",
            "2026-01-01T10:00:00Z"
          ): Unit
          (root / "guide[1].md").write("unindexed source\n")
        }
        info <- GitMetadata.markdown(source)
        footer = SourceFooter.markdown(info).adf.plainText
      } yield assertTrue(
        info.directory.relativePath.contains("guide[1].md"),
        info.changeStatus == GitMetadata.ChangeStatus.Uncommitted,
        info.history.isEmpty,
        footer.contains(
          "Source has uncommitted changes; history below contains committed changes only."
        ),
        footer.contains("No committed history for this source file.")
      )
    },
    test("clean committed source has no uncommitted note") {
      GitFixtures.temp
        .flatMap { dir =>
          def git(args: String*): Unit = GitFixtures.run(dir, args*)
          ZIO
            .attemptBlocking {
              git("init", "-q")
              git("config", "user.name", "Author")
              git("config", "user.email", "a@example.test")
              val source = dir / "clean.md"
              source.write("steady\n")
              git("add", "clean.md")
              git("commit", "-q", "-m", "initial")
              source
            }
            .flatMap { source =>
              GitMetadata.markdown(source).map { info =>
                assertTrue(
                  info.changeStatus == GitMetadata.ChangeStatus.Clean,
                  info.history.nonEmpty
                )
              }
            }
        }
    },
    test(
      "unstaged edits are uncommitted and other files stay clean when reverted"
    ) {
      GitFixtures.temp
        .flatMap { dir =>
          def git(args: String*): Unit = GitFixtures.run(dir, args*)
          for {
            pair <- ZIO.attemptBlocking {
              git("init", "-q")
              git("config", "user.name", "Author")
              git("config", "user.email", "a@example.test")
              val touched = dir / "touched.md"
              val other = dir / "other.md"
              touched.write("steady\n")
              other.write("steady\n")
              git("add", "touched.md", "other.md")
              git("commit", "-q", "-m", "initial")
              touched.appendLine("dirty line")
              (touched, other)
            }
            (touched, other) = pair
            dirty <- GitMetadata.markdown(touched)
            cleanOther <- GitMetadata.markdown(other)
            _ <- ZIO.attemptBlocking(git("checkout", "--", "touched.md"))
            afterRevert <- GitMetadata.markdown(touched)
          } yield assertTrue(
            dirty.changeStatus == GitMetadata.ChangeStatus.Uncommitted,
            cleanOther.changeStatus == GitMetadata.ChangeStatus.Clean,
            dirty.history.nonEmpty,
            afterRevert.changeStatus == GitMetadata.ChangeStatus.Clean
          )
        }
    },
    test("change status follows content, not stat data") {
      for {
        dir <- GitFixtures.temp
        source <- ZIO.attemptBlocking {
          GitFixtures.init(dir)
          GitFixtures.commitFile(
            dir,
            "guide.md",
            "committed\n",
            "Add guide",
            "2026-01-01T10:00:00Z"
          )
        }
        status = GitMetadata.markdown(source).map(_.changeStatus)
        _ <- ZIO.attemptBlocking(
          source.touch(Instant.now().plusSeconds(60))
        )
        touched <- status
        _ <- ZIO.attemptBlocking {
          source.write("staged\n")
          GitFixtures.run(dir, "add", "guide.md")
        }
        stagedOnly <- status
        _ <- ZIO.attemptBlocking(source.write("committed\n"))
        stagedThenReverted <- status
        _ <- ZIO.attemptBlocking {
          GitFixtures.run(dir, "reset", "-q", "--hard")
          source.write("unstaged\n")
        }
        unstagedOnly <- status
        _ <- ZIO.attemptBlocking {
          source.write("staged again\n")
          GitFixtures.run(dir, "add", "guide.md")
          source.write("and unstaged\n")
        }
        both <- status
      } yield assertTrue(
        touched == GitMetadata.ChangeStatus.Clean,
        stagedOnly == GitMetadata.ChangeStatus.Uncommitted,
        stagedThenReverted == GitMetadata.ChangeStatus.Clean,
        unstagedOnly == GitMetadata.ChangeStatus.Uncommitted,
        both == GitMetadata.ChangeStatus.Uncommitted
      )
    },
    test("unborn HEAD shows no history without a status failure") {
      GitFixtures.temp
        .flatMap { dir =>
          def git(args: String*): Unit = GitFixtures.run(dir, args*)
          ZIO
            .attemptBlocking {
              git("init", "-q")
              val source = dir / "draft.md"
              source.write("never committed\n")
              git("add", "draft.md")
              source
            }
            .flatMap { source =>
              GitMetadata.markdown(source).map { info =>
                val footer = SourceFooter.markdown(info)
                val expected = SourceFooter.markdown(
                  GitMetadata.MarkdownInfo(
                    GitMetadata.DirectoryInfo(
                      None,
                      Some("draft.md"),
                      Some(
                        "Clone command unavailable: no Git remote configured."
                      )
                    ),
                    Nil,
                    changeStatus = GitMetadata.ChangeStatus.Uncommitted
                  )
                )
                assertTrue(
                  info.changeStatus == GitMetadata.ChangeStatus.Uncommitted,
                  info.history.isEmpty,
                  info.historyNotice.isEmpty,
                  footer.adf.toString == expected.adf.toString,
                  footer.hashInput == expected.hashInput
                )
              }
            }
        }
    },
    test(
      "discovery distinguishes outside a worktree from a broken Git config"
    ) {
      for {
        root <- GitFixtures.temp
        repo = root / "repo"
        _ <- ZIO.attemptBlocking {
          GitFixtures.init(repo)
          (repo / ".git" / "config").write("[broken\n")
        }
        outside <- GitMetadata.directory(root)
        broken <- GitMetadata.directory(repo)
      } yield assertTrue(
        outside.gitNotice.contains(
          "Git metadata unavailable: source is not in a Git worktree."
        ),
        broken.gitNotice.contains(
          "Git metadata unavailable: repository lookup failed."
        )
      )
    },
    test(
      "bare repositories and broken worktree markers have distinct notices"
    ) {
      for {
        root <- GitFixtures.temp
        bare = root / "bare"
        worktree = root / "worktree"
        sourceDir = worktree / "docs"
        _ <- ZIO.attemptBlocking {
          GitFixtures.run(root, "init", "--bare", bare.name)
          GitFixtures.init(worktree)
          sourceDir.createDirectory(): Unit
          (worktree / ".git" / "HEAD").write("invalid\n"): Unit
        }
        bareInfo <- GitMetadata.directory(bare)
        broken <- GitMetadata.directory(sourceDir)
      } yield assertTrue(
        bareInfo.gitNotice.contains(
          "Git metadata unavailable: source is not in a Git worktree."
        ),
        broken.gitNotice.contains(
          "Git metadata unavailable: repository lookup failed."
        )
      )
    },
    test("a malformed branch ref does not look like an unborn HEAD") {
      for {
        root <- GitFixtures.temp
        source <- ZIO.attemptBlocking {
          GitFixtures.init(root)
          val source = GitFixtures.commitFile(
            root,
            "guide.md",
            "body\n",
            "Add guide",
            "2026-01-01T10:00:00Z"
          )
          (root / ".git" / "refs" / "heads" / "main").write("malformed\n")
          source
        }
        info <- GitMetadata.markdown(source)
      } yield assertTrue(
        info.directory.relativePath.contains("guide.md"),
        info.changeStatus == GitMetadata.ChangeStatus.Unavailable,
        info.historyNotice.contains("Source history unavailable.")
      )
    }
  ) @@ GitFixtures.isolated
}
