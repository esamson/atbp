package ph.samson.atbp.cli

import better.files.File
import zio.ZIO
import zio.cli.CliConfig
import zio.cli.CommandDirective
import zio.test.*

object PlateParallelismSpec extends ZIOSpecDefault {

  private def parse(args: List[String]): ZIO[Any, Throwable, Plate] =
    Plate.command
      .parse("plate" :: args, CliConfig.default)
      .flatMap {
        case CommandDirective.UserDefined(_, plate) => ZIO.succeed(plate)
        case CommandDirective.BuiltIn(_)            =>
          ZIO.fail(new Exception("expected a Plate value"))
      }

  private def withSource(
      use: String => ZIO[Any, Throwable, TestResult]
  ): ZIO[Any, Throwable, TestResult] =
    ZIO.acquireReleaseWith(
      ZIO.attemptBlockingIO(File.newTemporaryFile("plate", ".md"))
    )(file => ZIO.attemptBlockingIO(file.delete()).orDie) { file =>
      use(file.pathAsString)
    }

  override def spec = suite("Plate parallelism")(
    test("default check uses parallelism 10") {
      withSource { path =>
        parse(List("check", "--status", "cooking", path)).map { plate =>
          assertTrue(plate.parallel == Plate.DefaultParallelism)
        }
      }
    },
    test("custom parallelism before check subcommand") {
      withSource { path =>
        parse(
          List("--parallel", "100", "check", "--status", "stale", path)
        ).map { plate =>
          assertTrue(plate.parallel == 100)
        }
      }
    },
    test("option after the check subcommand does not parse") {
      withSource { path =>
        parse(List("check", "--parallel", "5", "--status", "done", path)).exit
          .map { exit =>
            assertTrue(exit.isFailure)
          }
      }
    },
    test("default label uses parallelism 10") {
      withSource { path =>
        parse(List("label", "--value", "x", path)).map { plate =>
          assertTrue(plate.parallel == Plate.DefaultParallelism)
        }
      }
    },
    test("custom parallelism before radar") {
      withSource { path =>
        parse(List("--parallel", "3", "radar", path)).map { plate =>
          assertTrue(plate.parallel == 3)
        }
      }
    },
    test("custom parallelism before sort") {
      withSource { path =>
        parse(List("--parallel", "1", "sort", path)).map { plate =>
          assertTrue(plate.parallel == 1)
        }
      }
    }
  )
}
