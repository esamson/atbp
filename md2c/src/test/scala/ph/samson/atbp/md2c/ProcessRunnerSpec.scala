package ph.samson.atbp.md2c

import zio.ZIO
import zio.test.*

object ProcessRunnerSpec extends ZIOSpecDefault {
  import GitSpecSupport.*

  override def spec = suite("ProcessRunner")(
    suite("process runner")(
      test("sets noninteractive environment and closes stdin") {
        withScripts { dir =>
          val script = executable(
            dir / "git",
            "#!/bin/sh\necho \"$GIT_TERMINAL_PROMPT:$GIT_PAGER:$GIT_OPTIONAL_LOCKS\"\ncat\n"
          )
          processRunner(script.path.toString).run(dir, Nil).map { result =>
            assertTrue(
              result.exitCode == 0,
              result.output == "0:cat:0\n"
            )
          }
        }
      },
      test(
        "isolated runner replaces personal config and repository variables"
      ) {
        withScripts { dir =>
          val script = executable(
            dir / "git",
            "#!/bin/sh\necho \"$GIT_CONFIG_GLOBAL:$GIT_CONFIG_NOSYSTEM:$GIT_CONFIG_COUNT:${GIT_CONFIG_PARAMETERS-unset}:${GIT_DIR-unset}:${GIT_CONFIG-unset}\"\n"
          )
          ProcessRunner
            .processRunner(
              script.path.toString,
              ProcessRunner.DefaultTimeoutMillis,
              ProcessRunner.DefaultMaxOutputBytes,
              ProcessRunner.DefaultConcurrency,
              env => {
                env.put("GIT_CONFIG", "inherited-config")
                GitFixtures.isolate(env)
              }
            )
            .run(dir, Nil)
            .map(result =>
              assertTrue(result.output == "/dev/null:1:0:unset:unset:unset\n")
            )
        }
      },
      test("blocks transports even when lazy-fetch suppression is ignored") {
        withScripts { dir =>
          for {
            script <- ZIO.attemptBlocking {
              GitFixtures.init(dir)
              executable(
                dir / "check-transport",
                """#!/bin/sh
                  |printf '%s\n' "$GIT_NO_LAZY_FETCH"
                  |unset GIT_NO_LAZY_FETCH
                  |exec git -c protocol.file.allow=always ls-remote .
                  |""".stripMargin
              )
            }
            result <- processRunner(script.path.toString).run(dir, Nil)
          } yield assertTrue(result.exitCode == 128, result.output == "1\n")
        }
      },
      test("reports exit codes and discards stderr") {
        withScripts { dir =>
          val script = executable(
            dir / "git",
            "#!/bin/sh\necho warning-secret >&2\necho out\nexit 3\n"
          )
          processRunner(script.path.toString).run(dir, Nil).map { result =>
            assertTrue(
              result == GitMetadata.CommandResult(3, "out\n")
            )
          }
        }
      },
      test("drains oversized discovery diagnostics without classifying them") {
        withScripts { dir =>
          val script = executable(
            dir / "git",
            """#!/bin/sh
              |echo 'fatal: not a git repository (or any of the parent directories): .git' >&2
              |head -c 200000 /dev/zero | tr '\0' 'x' >&2
              |echo out
              |exit 128
              |""".stripMargin
          )
          processRunner(script.path.toString)
            .run(dir, List("rev-parse", "--show-toplevel"))
            .map(result =>
              assertTrue(result == GitMetadata.CommandResult(128, "out\n"))
            )
        }
      },
      test("a missing executable is reported as unavailable") {
        withScripts { dir =>
          processRunner((dir / "no-such-git").path.toString)
            .run(dir, Nil)
            .either
            .map(result =>
              assertTrue(
                result.left.toOption
                  .exists {
                    case _: GitMetadata.ExecutableUnavailable => true;
                    case _                                    => false
                  }
              )
            )
        }
      },
      test("missing git degrades to the specified notices end to end") {
        withScripts { dir =>
          val source = dir / "guide.md"
          source.write("hello\n")
          val runner = processRunner((dir / "no-such-git").path.toString)
          GitMetadata.markdown(source, runner).map { info =>
            val footer = SourceFooter.markdown(info)
            assertTrue(
              info.directory.gitNotice.contains(
                "Git metadata unavailable: git executable not available."
              ),
              footer.adf.plainText.contains(
                "Do not edit here. This page was generated from markdown source."
              ),
              footer.hashInput == SourceFooter.frame(
                List(
                  "md2c-source-footer-v1",
                  "Do not edit here. This page was generated from markdown source.",
                  "Git metadata unavailable: git executable not available."
                )
              )
            )
          }
        }
      },
      test("a hung command times out and its process tree is reaped") {
        withScripts { dir =>
          val pidFile = dir / "child.pid"
          val script = executable(
            dir / "git",
            s"#!/bin/sh\nsleep 30 &\necho $$! > '${pidFile.path}'\nwait\n"
          )
          for {
            started <- zio.Clock.nanoTime
            result <- timed(script.path.toString, 1500)
              .run(dir, Nil)
              .either
            finished <- zio.Clock.nanoTime
            dead <- processDead(pidFile)
          } yield assertTrue(
            result.left.toOption
              .exists { case _: GitMetadata.TimedOut => true; case _ => false },
            (finished - started) < 8000000000L,
            dead
          )
        }
      },
      test("timeouts degrade the affected fields like other failures") {
        withScripts { dir =>
          val source = dir / "guide.md"
          source.write("hello\n")
          val script = executable(dir / "git", "#!/bin/sh\nsleep 30\n")
          val runner = timed(script.path.toString, 200)
          GitMetadata.markdown(source, runner).map { info =>
            assertTrue(
              info.directory.gitNotice.contains(
                "Git metadata unavailable: repository lookup failed."
              )
            )
          }
        }
      },
      test("interruption terminates the running command") {
        withScripts { dir =>
          val pidFile = dir / "child.pid"
          val script = executable(
            dir / "git",
            s"#!/bin/sh\nsleep 30 &\necho $$! > '${pidFile.path}'\nwait\n"
          )
          for {
            fiber <- timed(script.path.toString, 60000)
              .run(dir, Nil)
              .fork
            _ <- awaitFile(pidFile)
            _ <- fiber.interrupt
            dead <- processDead(pidFile)
          } yield assertTrue(dead)
        }
      },
      test("large stdout and stderr do not deadlock") {
        withScripts { dir =>
          val noisy = executable(
            dir / "noisy",
            "#!/bin/sh\nhead -c 5000000 /dev/zero | tr '\\0' 'e' >&2\necho ok\n"
          )
          val flood = executable(
            dir / "flood",
            "#!/bin/sh\nhead -c 5000000 /dev/zero | tr '\\0' 'a'\n"
          )
          for {
            quiet <- timed(noisy.path.toString, 8000)
              .run(dir, Nil)
            capped <- configured(
              flood.path.toString,
              8000,
              1000,
              ProcessRunner.DefaultConcurrency
            )
              .run(dir, Nil)
              .either
          } yield assertTrue(
            quiet == GitMetadata.CommandResult(0, "ok\n"),
            capped.left.toOption.exists {
              case failure: GitMetadata.Failed =>
                failure.category == "output-limit"
              case _ => false
            }
          )
        }
      },
      test("concurrent commands are bounded") {
        withScripts { dir =>
          val log = dir / "log"
          val script = executable(
            dir / "git",
            s"#!/bin/sh\necho s >> '${log.path}'\nsleep 0.3\necho e >> '${log.path}'\necho ok\n"
          )
          val runner =
            configured(
              script.path.toString,
              ProcessRunner.DefaultTimeoutMillis,
              ProcessRunner.DefaultMaxOutputBytes,
              2
            )
          for {
            results <- ZIO.foreachPar(1 to 6)(_ => runner.run(dir, Nil))
            events = log.lines.toList
            peak = events
              .scanLeft(0)((n, e) => if (e == "s") n + 1 else n - 1)
              .max
          } yield assertTrue(
            results.forall(_.output == "ok\n"),
            events.count(_ == "s") == 6,
            peak == 2
          )
        }
      }
    ) @@ TestAspect.withLiveClock @@ TestAspect.sequential
  ) @@ GitFixtures.isolated
}
