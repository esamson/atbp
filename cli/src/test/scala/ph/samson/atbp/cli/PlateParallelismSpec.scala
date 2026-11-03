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
    test("default check is serial") {
      withSource { path =>
        parse(List("check", "--status", "cooking", path)).map { plate =>
          assertTrue(!plate.parallel)
        }
      }
    },
    test("opt-in check is parallel") {
      withSource { path =>
        parse(
          List("--parallel", "check", "--status", "stale", path)
        ).map { plate =>
          assertTrue(plate.parallel)
        }
      }
    },
    test("flag after the check subcommand does not parse") {
      withSource { path =>
        parse(List("check", "--parallel", "--status", "done", path)).exit
          .map { exit =>
            assertTrue(exit.isFailure)
          }
      }
    },
    test("default label is serial") {
      withSource { path =>
        parse(List("label", "--value", "x", path)).map { plate =>
          assertTrue(!plate.parallel)
        }
      }
    },
    test("opt-in radar is parallel") {
      withSource { path =>
        parse(List("--parallel", "radar", path)).map { plate =>
          assertTrue(plate.parallel)
        }
      }
    },
    test("opt-in sort is parallel") {
      withSource { path =>
        parse(List("--parallel", "sort", path)).map { plate =>
          assertTrue(plate.parallel)
        }
      }
    },
    test("jiraParallelism maps the flag to 1 or 100") {
      assertTrue(
        Plate.jiraParallelism(false) == 1,
        Plate.jiraParallelism(true) == 100
      )
    }
  )
}
