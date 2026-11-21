package ph.samson.atbp.md2c

import better.files.File
import zio.ZIO

import java.util.concurrent.atomic.AtomicReference

/** Deterministic Git command results for failure and recovery tests. */
private[md2c] object GitFakes {
  val RemoteUrl = "git@example.com:team/docs.git"
  val ObjectId = "0123456789abcdef0123456789abcdef01234567"

  def key(arguments: List[String]): String = arguments match {
    case "rev-parse" :: "--show-toplevel" :: _         => "toplevel"
    case "rev-parse" :: "--is-shallow-repository" :: _ => "shallow"
    case "rev-parse" :: _                              => "head"
    case first :: _                                    => first
    case Nil                                           => ""
  }

  /** Healthy repository at `root`; `failures` replaces results per command. */
  class ScriptedRunner(
      root: File,
      initial: Map[String, Throwable] = Map.empty,
      exitCodes: Map[String, Int] = Map.empty,
      lsTreeOutput: Option[String] = None
  ) extends GitMetadata.Runner {
    val failures = new AtomicReference(initial)
    private val captured = new AtomicReference(List.empty[List[String]])

    def calls: List[List[String]] = captured.get()

    def fail(newFailures: Map[String, Throwable]): Unit =
      failures.set(newFailures)

    private def ok(output: String) =
      ZIO.succeed(GitMetadata.CommandResult(0, output))

    override def run(directory: File, arguments: List[String]) = {
      captured.updateAndGet(_ :+ arguments)
      failures.get().get(key(arguments)) match {
        case Some(error)                                => ZIO.fail(error)
        case None if exitCodes.contains(key(arguments)) =>
          ZIO.succeed(
            GitMetadata.CommandResult(exitCodes(key(arguments)), "")
          )
        case None =>
          key(arguments) match {
            case "toplevel" => ok(root.path.toString)
            case "remote"   => ok("origin\n")
            case "ls-tree"  =>
              ok(
                lsTreeOutput.getOrElse(
                  s"100644 blob $ObjectId\t${arguments.last}\u0000"
                )
              )
            case "config" => ok(s"$RemoteUrl\u0000")
            case "log"    =>
              ok(
                "2026-10-17\u0000Alex Cruz\u00000123456\u0000Clarify setup\u0000\n"
              )
            case _ => ok("")
          }
      }
    }
  }
}
