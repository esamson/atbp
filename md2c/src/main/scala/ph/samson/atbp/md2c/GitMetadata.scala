package ph.samson.atbp.md2c

import better.files.File
import zio.FiberRef
import zio.Task
import zio.Unsafe
import zio.ZIO

import java.nio.file.Files
import java.nio.file.LinkOption.NOFOLLOW_LINKS

object GitMetadata {
  private[md2c] val CloneLookupFailedNotice =
    "Clone command unavailable: remote lookup failed."

  case class DirectoryInfo(
      cloneCommand: Option[String],
      relativePath: Option[String],
      cloneNotice: Option[String] = None,
      gitNotice: Option[String] = None
  )
  case class HistoryEntry(
      date: String,
      author: String,
      hash: String,
      subject: String
  )
  enum ChangeStatus {
    case Clean, Uncommitted, Unavailable
  }
  case class MarkdownInfo(
      directory: DirectoryInfo,
      history: List[HistoryEntry],
      historyNotice: Option[String] = None,
      changeStatus: ChangeStatus = ChangeStatus.Clean,
      shallow: Boolean = false
  )

  private[md2c] enum Diagnostic {
    case RepositoryNotFound, BareRepository, UnresolvedHead
  }
  private[md2c] case class CommandResult(
      exitCode: Int,
      output: String,
      diagnostic: Option[Diagnostic] = None
  )
  private[md2c] trait Runner {
    def run(directory: File, arguments: List[String]): Task[CommandResult]
  }

  /** Command failures carry only a fixed category: never raw diagnostics. */
  private[md2c] sealed abstract class CommandFailure(val category: String)
      extends RuntimeException(category) {
    override def fillInStackTrace(): Throwable = this
  }
  private[md2c] final class ExecutableUnavailable
      extends CommandFailure("executable-unavailable")
  private[md2c] final class TimedOut extends CommandFailure("timeout")
  private[md2c] final class Failed(category: String)
      extends CommandFailure(category)

  private val defaultRunner = ProcessRunner.processRunner(
    "git",
    ProcessRunner.DefaultTimeoutMillis,
    ProcessRunner.DefaultMaxOutputBytes,
    ProcessRunner.DefaultConcurrency
  )

  private val currentRunner: FiberRef[Runner] =
    Unsafe.unsafe(implicit u => FiberRef.unsafe.make[Runner](defaultRunner))

  /** Package-private injection point for deterministic failure tests. */
  private[md2c] def withRunner[R, E, A](runner: Runner)(
      effect: ZIO[R, E, A]
  ): ZIO[R, E, A] = currentRunner.locally(runner)(effect)

  /** Log only fixed categories: never commands, URLs, paths, or stderr. */
  private def guarded(runner: Runner): Runner = new Runner {
    override def run(
        directory: File,
        arguments: List[String]
    ): Task[CommandResult] = runner
      .run(directory, arguments)
      .catchAllDefect(ZIO.fail(_))
      .tapBoth(
        {
          case failure: CommandFailure =>
            ZIO.logWarning(
              s"Git metadata command failed: subcommand=${subcommand(arguments)} reason=${failure.category}"
            )
          case _: java.io.IOException =>
            ZIO.logWarning(
              s"Git metadata command failed: subcommand=${subcommand(arguments)} reason=io-error"
            )
          case _ =>
            ZIO.logWarning(
              s"Git metadata command failed: subcommand=${subcommand(arguments)} reason=error"
            )
        },
        result =>
          ZIO.when(result.exitCode > 1)(
            ZIO.logDebug(
              s"Git metadata command exited: subcommand=${subcommand(arguments)} exit=${result.exitCode}"
            )
          )
      )
  }

  private val knownSubcommands =
    Set(
      "rev-parse",
      "symbolic-ref",
      "show-ref",
      "remote",
      "config",
      "ls-files",
      "ls-tree",
      "diff",
      "log"
    )

  private def subcommand(arguments: List[String]): String =
    arguments.headOption.filter(knownSubcommands).getOrElse("unknown")

  def directory(source: File): Task[DirectoryInfo] =
    currentRunner.get.flatMap(directory(source, _))

  def markdown(source: File): Task[MarkdownInfo] =
    currentRunner.get.flatMap(markdown(source, _))

  private enum HeadStatus {
    case Present, Unborn, Unavailable
  }

  private def headStatus(root: File, runner: Runner): Task[HeadStatus] =
    runner.run(root, List("rev-parse", "--verify", "HEAD")).either.flatMap {
      case Right(result) if result.exitCode == 0 =>
        ZIO.succeed(HeadStatus.Present)
      case Right(result)
          if result.diagnostic.contains(Diagnostic.UnresolvedHead) =>
        runner
          .run(root, List("symbolic-ref", "--quiet", "HEAD"))
          .flatMap { symbolic =>
            val ref = symbolic.output.stripLineEnd
            if (symbolic.exitCode == 0 && ref.startsWith("refs/heads/"))
              runner
                .run(root, List("show-ref", "--verify", "--quiet", ref))
                .map(result =>
                  if (result.exitCode == 1) HeadStatus.Unborn
                  else HeadStatus.Unavailable
                )
            else ZIO.succeed(HeadStatus.Unavailable)
          }
          .orElseSucceed(HeadStatus.Unavailable)
      case _ => ZIO.succeed(HeadStatus.Unavailable)
    }

  private[md2c] def markdown(
      source: File,
      runner: Runner
  ): Task[MarkdownInfo] = {
    val commands = guarded(runner)
    worktreeRoot(source, commands).flatMap {
      case Left(notice) =>
        ZIO.succeed(
          historyUnavailable(
            DirectoryInfo(None, None, gitNotice = Some(notice)),
            ChangeStatus.Unavailable
          )
        )
      case Right(root) =>
        directoryInfo(source, root, commands).flatMap { info =>
          info.relativePath match {
            case Some(path) => markdownFile(info, root, path, commands)
            case None       =>
              ZIO.succeed(historyUnavailable(info, ChangeStatus.Unavailable))
          }
        }
    }
  }

  private def historyUnavailable(
      info: DirectoryInfo,
      changeStatus: ChangeStatus
  ): MarkdownInfo =
    MarkdownInfo(info, Nil, Some("Source history unavailable."), changeStatus)

  private def markdownFile(
      info: DirectoryInfo,
      root: File,
      path: String,
      runner: Runner
  ): Task[MarkdownInfo] =
    runner
      .run(root, List("ls-files", "--error-unmatch", "--", path))
      .either
      .flatMap {
        case Right(result) if result.exitCode == 1 =>
          ZIO.succeed(
            MarkdownInfo(info, Nil, changeStatus = ChangeStatus.Uncommitted)
          )
        case index =>
          val tracked = index.exists(_.exitCode == 0)
          headStatus(root, runner).flatMap {
            case HeadStatus.Unavailable =>
              ZIO.succeed(historyUnavailable(info, ChangeStatus.Unavailable))
            case HeadStatus.Unborn =>
              val changeStatus =
                if (tracked) ChangeStatus.Uncommitted
                else ChangeStatus.Unavailable
              ZIO.succeed(MarkdownInfo(info, Nil, changeStatus = changeStatus))
            case HeadStatus.Present if tracked =>
              sourceChangeStatus(root, path, runner).flatMap(status =>
                sourceHistory(info, root, path, status, runner)
              )
            case HeadStatus.Present =>
              // After an index failure, history may belong to an earlier file at this path.
              regularFileInHead(root, path, runner).flatMap {
                case true =>
                  sourceHistory(
                    info,
                    root,
                    path,
                    ChangeStatus.Unavailable,
                    runner
                  )
                case false =>
                  ZIO.succeed(
                    historyUnavailable(info, ChangeStatus.Unavailable)
                  )
              }
          }
      }

  private val RegularBlobHeader =
    "(?:100644|100755) blob (?:[0-9a-f]{40}|[0-9a-f]{64})".r

  /** Split the metadata header from the raw filename, which can contain tabs.
    */
  private def regularFileInHead(
      root: File,
      path: String,
      runner: Runner
  ): Task[Boolean] =
    runner
      .run(root, List("ls-tree", "-z", "HEAD", "--", path))
      .map { result =>
        val tab = result.output.indexOf('\t')
        result.exitCode == 0 && tab >= 0 &&
        RegularBlobHeader.matches(result.output.substring(0, tab)) &&
        result.output.substring(tab + 1) == s"$path\u0000"
      }
      .orElseSucceed(false)

  private def sourceChangeStatus(
      root: File,
      path: String,
      runner: Runner
  ): Task[ChangeStatus] =
    runner
      .run(root, List("diff", "--quiet", "HEAD", "--", path))
      .either
      .map {
        case Right(result) if result.exitCode == 0 => ChangeStatus.Clean
        case Right(result) if result.exitCode == 1 =>
          ChangeStatus.Uncommitted
        case _ => ChangeStatus.Unavailable
      }

  private def sourceHistory(
      info: DirectoryInfo,
      root: File,
      path: String,
      changeStatus: ChangeStatus,
      runner: Runner
  ): Task[MarkdownInfo] =
    runner
      .run(
        root,
        List(
          "log",
          "--no-show-signature",
          "--follow",
          "-5",
          "--date=format:%Y-%m-%d",
          "--format=%ad%x00%an%x00%h%x00%s%x00",
          "--",
          path
        )
      )
      .either
      .flatMap {
        case Right(result) if result.exitCode == 0 =>
          parseHistory(result.output) match {
            case Some(history) =>
              isShallow(root, runner).when(history.nonEmpty).map { shallow =>
                MarkdownInfo(
                  info,
                  history,
                  changeStatus = changeStatus,
                  shallow = shallow.contains(true)
                )
              }
            case None => ZIO.succeed(historyUnavailable(info, changeStatus))
          }
        case _ => ZIO.succeed(historyUnavailable(info, changeStatus))
      }

  /** Local lookup only; a failed or unsupported query claims nothing. */
  private def isShallow(root: File, runner: Runner): Task[Boolean] =
    runner
      .run(root, List("rev-parse", "--is-shallow-repository"))
      .map(result => result.exitCode == 0 && result.output.trim == "true")
      .orElseSucceed(false)

  private val HistoryDate = "\\d{4}-\\d{2}-\\d{2}"
  private val HistoryHash = "[0-9a-f]+"

  /** Require four NUL-terminated fields and a newline for every record. */
  private def parseHistory(output: String): Option[List[HistoryEntry]] = {
    if (output.isEmpty) Some(Nil)
    else if (!output.endsWith("\u0000\n")) None
    else {
      val fields = output.split("\\u0000", -1).toList.dropRight(1)
      val records = fields
        .grouped(4)
        .zipWithIndex
        .map {
          case (List(date, author, hash, subject), index) =>
            // The newline terminating the previous record precedes this date.
            val datePattern = if (index == 0) HistoryDate else s"\n$HistoryDate"
            Option.when(date.matches(datePattern) && hash.matches(HistoryHash))(
              HistoryEntry(
                date.stripPrefix("\n"),
                author,
                hash,
                if (subject.isEmpty) "(no subject)" else subject
              )
            )
          case _ => None
        }
        .toList
      Option.when(records.forall(_.isDefined))(records.flatten)
    }
  }

  private[md2c] def directory(
      source: File,
      runner: Runner
  ): Task[DirectoryInfo] = {
    val commands = guarded(runner)
    worktreeRoot(source, commands).flatMap {
      case Left(notice) =>
        ZIO.succeed(DirectoryInfo(None, None, gitNotice = Some(notice)))
      case Right(root) => directoryInfo(source, root, commands)
    }
  }

  private def confirmsNoWorktree(
      directory: File,
      diagnostic: Option[Diagnostic]
  ): Task[Boolean] = diagnostic match {
    case Some(Diagnostic.BareRepository)     => ZIO.succeed(true)
    case Some(Diagnostic.RepositoryNotFound) =>
      // Git also reports "not a git repository" for malformed .git markers.
      ZIO
        .attemptBlocking {
          Iterator
            .iterate(Option(directory.path.toAbsolutePath))(
              _.flatMap(path => Option(path.getParent))
            )
            .takeWhile(_.nonEmpty)
            .flatten
            .forall(path =>
              Files.notExists(path.resolve(".git"), NOFOLLOW_LINKS)
            )
        }
        .orElseSucceed(false)
    case _ => ZIO.succeed(false)
  }

  private def worktreeRoot(
      source: File,
      runner: Runner
  ): Task[Either[String, File]] = {
    for {
      isDirectory <- ZIO.attemptBlocking(source.isDirectory)
      sourceDir = if (isDirectory) source else source.parent
      result <- lookupWorktreeRoot(sourceDir, runner)
    } yield result
  }

  private def lookupWorktreeRoot(
      sourceDir: File,
      runner: Runner
  ): Task[Either[String, File]] = {
    runner.run(sourceDir, List("rev-parse", "--show-toplevel")).either.flatMap {
      case Left(error) =>
        val notice = error match {
          case _: ExecutableUnavailable =>
            "Git metadata unavailable: git executable not available."
          case _ => "Git metadata unavailable: repository lookup failed."
        }
        ZIO.succeed(Left(notice))
      case Right(rootResult) if rootResult.exitCode != 0 =>
        confirmsNoWorktree(sourceDir, rootResult.diagnostic).map { outside =>
          Left(
            if (outside)
              "Git metadata unavailable: source is not in a Git worktree."
            else "Git metadata unavailable: repository lookup failed."
          )
        }
      case Right(rootResult) => ZIO.succeed(Right(File(rootResult.output.trim)))
    }
  }

  private def directoryInfo(
      source: File,
      root: File,
      runner: Runner
  ): Task[DirectoryInfo] = {
    val lookupFailed = CloneLookupFailedNotice
    for {
      resolved <- ZIO.attemptBlocking {
        val rootPath = root.path.toRealPath()
        val sourcePath = source.path.toRealPath()
        if (sourcePath.startsWith(rootPath))
          Some(
            rootPath
              .relativize(sourcePath)
              .toString
              .replace(java.io.File.separatorChar, '/')
          )
        else None
      }.either
      relative = resolved.toOption.flatten
      _ <- ZIO.when(resolved.isLeft)(
        ZIO.logWarning(
          "Git metadata path unavailable: source path could not be relativized."
        )
      )
      remotes <- runner.run(root, List("remote")).either
      names = remotes.toOption
        .filter(_.exitCode == 0)
        .map(_.output.linesIterator.map(_.trim).filter(_.nonEmpty).toList)
      remote = names.flatMap(CloneUrl.selectRemote)
      rawUrl <- remote match {
        case Some(name) =>
          runner
            .run(
              root,
              List("config", "--null", "--get-all", s"remote.$name.url")
            )
            .either
            .map(
              _.toOption
                .filter(_.exitCode == 0)
                .flatMap { result =>
                  val end = result.output.indexOf('\u0000')
                  Option.when(end >= 0)(result.output.substring(0, end))
                }
            )
        case None => ZIO.succeed(None)
      }
      clone = rawUrl.flatMap(CloneUrl.safeCloneCommand)
      finalNotice = names match {
        case None      => Some(lookupFailed)
        case Some(Nil) =>
          Some("Clone command unavailable: no Git remote configured.")
        case Some(_) if remote.isEmpty =>
          Some(
            "Clone command unavailable: multiple Git remotes and no origin."
          )
        case Some(_) if rawUrl.isEmpty => Some(lookupFailed)
        case Some(_) if clone.isEmpty  =>
          Some(
            "Clone command unavailable: selected remote URL contains credentials or is unusable."
          )
        case _ => None
      }
    } yield DirectoryInfo(clone, relative, finalNotice)
  }
}
