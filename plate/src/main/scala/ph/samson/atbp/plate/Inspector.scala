package ph.samson.atbp.plate

import better.files.File
import ph.samson.atbp.jira.Client
import ph.samson.atbp.jira.model.Changelog
import ph.samson.atbp.jira.model.Comment
import ph.samson.atbp.jira.model.Issue
import ph.samson.atbp.plate.JiraOps.*
import zio.Clock
import zio.Task
import zio.ZIO
import zio.ZLayer

import java.time.Instant
import java.time.LocalTime
import java.time.ZoneId
import java.time.temporal.ChronoUnit.DAYS
import scala.annotation.tailrec
import scala.util.control.NoStackTrace

trait Inspector {
  def cooking(source: File, target: Option[File]): Task[File]
  def stale(source: File, target: Option[File]): Task[File]
  def done(source: File, target: Option[File]): Task[File]
}

object Inspector {

  val JiraLink = """.*\(https://.*/browse/([A-Z]+-\d+)\).*""".r
  val CookingProgressDays = 14
  val StaleProgressDays = 28

  private def includeLine(
      check: String => Task[Boolean]
  )(line: String): Task[Boolean] = {
    line match {
      case ""                                 => ZIO.succeed(true)
      case heading if heading.startsWith("#") => ZIO.succeed(true)
      case JiraLink(key)                      => check(key)
      case _                                  => ZIO.succeed(false)
    }
  }

  private class LiveImpl(client: Client) extends Inspector {

    /** Extract relevant lines from the given source file and write to a sibling
      * file named with the given suffix.
      *
      * @param source
      *   source file
      * @param suffix
      *   suffix for naming the target file
      * @param check
      *   criteria for deciding if a Jira key should be included
      * @return
      *   target file
      */
    private def extract(source: File, target: File)(
        check: String => Task[Boolean]
    ): Task[File] =
      for {
        sourceLines <- ZIO.attemptBlockingIO(source.lines.toList)
        targetLines <- ZIO.filterPar(sourceLines)(includeLine(check))
        outFile <- ZIO.attemptBlockingIO {
          target.overwrite(prune(targetLines).mkString("\n"))
        }
      } yield outFile

    case class Enriched[T](line: String, aux: Option[T])
    private def unenriched[T](line: String) = Enriched[T](line, None)

    private def report[T](source: File, target: File)(
        enrichLine: String => Task[Enriched[T]],
        synthesize: List[Enriched[T]] => List[String]
    ): Task[File] =
      for {
        sourceLines <- ZIO.attemptBlockingIO(source.lines.toList)
        enrichedLines <- ZIO.foreachPar(sourceLines)(enrichLine)
        reportLines = synthesize(enrichedLines)
        pruned = prune(reportLines)
        outFile <- ZIO.attemptBlockingIO {
          target.overwrite(pruned.mkString("\n"))
        }
      } yield outFile

    private def sibling(source: File, suffix: String) = {
      val name = source.`extension`() match {
        case Some(ext) =>
          source.nameWithoutExtension(includeAll = false) + suffix + ext
        case None => source.name + suffix
      }
      source.sibling(name)
    }

    case class IssueTree(issue: Issue, descendants: List[Issue])

    def enrichKey[T](
        enrich: String => Task[T]
    )(line: String): Task[Enriched[T]] =
      line match {
        case JiraLink(key) => enrich(key).map(t => Enriched(line, Some(t)))
        case _             => ZIO.succeed(unenriched[T](line))
      }

    override def done(source: File, target: Option[File]): Task[File] = {
      val enrichLine: String => Task[Enriched[IssueTree]] = enrichKey { key =>
        for {
          (issue, descendants) <- client
            .getIssue(key)
            .zipPar(client.getDescendants(key))
        } yield IssueTree(issue, descendants)
      }

      def synthesize(lines: List[Enriched[IssueTree]]): List[String] = {
        @tailrec
        def process(
            remaining: List[Enriched[IssueTree]],
            succeedsIncludedJiraLine: Boolean, // true if last Jira line was included
            result: List[String]
        ): List[String] = remaining match {
          case Nil          => result.reverse
          case head :: next =>
            head match {
              case Enriched(line, None) =>
                line match {
                  case "" =>
                    process(next, succeedsIncludedJiraLine, "" :: result)
                  case heading if heading.startsWith("#") =>
                    process(next, false, heading :: result)
                  case other =>
                    if (succeedsIncludedJiraLine) {
                      process(next, true, other :: result)
                    } else {
                      process(next, false, result)
                    }
                }

              case Enriched(line, Some(IssueTree(issue, descendants))) =>
                if (issue.isDone) {
                  val reportLine =
                    if (issue.fields.resolution.exists(_.name == "Done")) {
                      line
                    } else {
                      val resolution = issue.fields.resolution
                        .map(_.name)
                        .getOrElse("NO RESOLUTION")
                      s"$line [RESOLVED as $resolution]"
                    }

                  if (descendants.forall(_.isDone)) {
                    process(next, true, reportLine :: result)
                  } else {
                    val notDone =
                      descendants.filterNot(_.isDone).map(_.key).mkString(", ")
                    val flagReport = s"$reportLine [But NOT DONE: $notDone]"
                    process(next, true, flagReport :: result)
                  }
                } else if (
                  descendants.nonEmpty && descendants.forall(_.isDone)
                ) {
                  val flagReport = s"$line [ALL DESCENDANTS DONE]"
                  process(next, true, flagReport :: result)
                } else {
                  process(next, false, result)
                }
            }
        }

        process(lines, false, Nil)
      }

      ZIO.logSpan("done") {
        report(source, target.getOrElse(sibling(source, ".done")))(
          enrichLine,
          synthesize
        )
      }
    }

    case class FatIssue(
        issue: Issue,
        changelogs: List[Changelog],
        comments: List[Comment]
    ) {
      def progress(sinceDays: Int)(using now: Instant): List[String] = {
        val limit = now
          .atZone(ZoneId.systemDefault())
          .minus(sinceDays, DAYS)
          .`with`(LocalTime.MIDNIGHT)

        val progressLogs: List[String] = changelogs.filter {
          case Changelog(id, author, created, items, historyMetadata) =>
            created.isAfter(limit) &&
            !items.forall { item =>
              // changes we don't count as progress
              item.field == "Rank"
              || item.field == "labels"
              || item.field == "Sprint"
              || (item.field == "status" && item.toAsString.contains("Todo"))
            }
        } flatMap {
          case Changelog(id, author, created, items, historyMetadata) =>
            val details = for {
              item <- items
              toValue = item.toAsString
                .getOrElse("NONE")
                .take(50)
                .replace("\n", "_n_")
            } yield {
              s"    * ${item.field} -> $toValue"
            }
            s"* <small>🚧</small> $created change by [${author.displayName}]" :: details
        }

        val progressComments = comments.filter {
          case Comment(
                self,
                id,
                author,
                updateAuthor,
                created,
                updated,
                renderedBody,
                jsdPublic
              ) =>
            updated.isAfter(limit)
        } map {
          case Comment(
                self,
                id,
                author,
                updateAuthor,
                created,
                updated,
                renderedBody,
                jsdPublic
              ) =>
            s"* <small>📝</small> $updated comment by [${updateAuthor.displayName}]"
        }

        progressLogs ++ progressComments
      }

      def hasProgress(sinceDays: Int)(using now: Instant): Boolean = {
        val limit = now
          .atZone(ZoneId.systemDefault())
          .minus(sinceDays, DAYS)
          .`with`(LocalTime.MIDNIGHT)

        val hasNewLogs = changelogs.exists {
          case Changelog(id, author, created, items, historyMetadata) =>
            created.isAfter(limit) &&
            !items.forall { item =>
              // changes we don't count as progress
              item.field == "Rank"
              || item.field == "labels"
              || item.field == "Sprint"
              || (item.field == "status" && item.toAsString.contains("Todo"))
            }
        }

        val hasNewComments = comments.exists {
          case Comment(
                self,
                id,
                author,
                updateAuthor,
                created,
                updated,
                renderedBody,
                jsdPublic
              ) =>
            updated.isAfter(limit)
        }

        issue.inProgress && (hasNewLogs || hasNewComments)
      }
    }

    object FatIssue {
      def of(issue: Issue) = for {
        (changelogs, comments) <- client
          .getChangelogs(issue.key)
          .zipPar(client.getComments(issue.key))
      } yield FatIssue(issue, changelogs, comments)
    }

    case class FatTree(issue: FatIssue, descendants: List[FatIssue])

    override def cooking(source: File, target: Option[File]): Task[File] = {
      val enrichLine: String => Task[Enriched[FatTree]] = enrichKey { key =>
        for {
          issueReq <- client.getIssue(key).fork
          descendantsReq <- client.getDescendants(key).fork
          issue <- issueReq.join
          fatIssueReq <- FatIssue.of(issue).fork
          descendants <- descendantsReq.join
          fatDescendantsReq <- ZIO.foreachPar(descendants)(FatIssue.of).fork
          (fatIssue, fatDescendants) <- fatIssueReq.join.zipPar(
            fatDescendantsReq.join
          )
        } yield FatTree(fatIssue, fatDescendants)
      }

      def synthesize(
          lines: List[Enriched[FatTree]]
      )(using now: Instant): List[String] = {
        @tailrec
        def process(
            remaining: List[Enriched[FatTree]],
            succeedsIncludedJiraLine: Boolean, // true if last Jira line was included
            result: List[String]
        ): List[String] = remaining match {
          case Nil          => result.reverse
          case head :: next =>
            head match {
              case Enriched(line, None) =>
                line match {
                  case "" =>
                    process(next, succeedsIncludedJiraLine, "" :: result)
                  case heading if heading.startsWith("#") =>
                    process(next, false, heading :: result)
                  case other =>
                    if (succeedsIncludedJiraLine) {
                      process(next, true, other :: result)
                    } else {
                      process(next, false, result)
                    }
                }

              case Enriched(line, Some(FatTree(fatIssue, descendants))) =>
                if (
                  !fatIssue.issue.isDone && (fatIssue.hasProgress(
                    CookingProgressDays
                  ) || descendants.exists(_.hasProgress(CookingProgressDays)))
                ) {
                  val indent =
                    " ".repeat(line.takeWhile(_.isWhitespace).length + 4)
                  val issueDetails = for {
                    detail <- fatIssue.progress(CookingProgressDays)
                  } yield {
                    s"$indent$detail"
                  }
                  val descendantDetails = for {
                    descendant <- descendants
                    if descendant.hasProgress(CookingProgressDays)
                  } yield {
                    val header =
                      s"$indent* <small>📌</small> [${descendant.issue.key} ${descendant.issue.fields.summary}](${descendant.issue.webUrl})"
                    val details = for {
                      detail <- descendant.progress(CookingProgressDays)
                    } yield {
                      s"$indent    $detail"
                    }
                    header :: details
                  }

                  val lines =
                    line :: (issueDetails ++ descendantDetails.flatten)
                  process(next, true, lines.reverse ++ result)
                } else {
                  process(next, false, result)
                }
            }
        }

        process(lines, false, Nil)
      }

      ZIO.logSpan("done") {
        for {
          now <- Clock.instant
          given Instant = now
          result <- report(source, target.getOrElse(sibling(source, ".done")))(
            enrichLine,
            synthesize
          )

        } yield result
      }

    }

    def Xcooking(source: File, target: Option[File]): Task[File] =
      ZIO.logSpan("cooking") {
        extract(source, target.getOrElse(sibling(source, ".cooking"))) { key =>
          for {
            issue <- client.getIssue(key)
            result <- ZIO.succeed(!issue.isDone) && (
              hasProgress(issue, CookingProgressDays)
                || anyDescendantHasProgress(issue, CookingProgressDays)
            )
          } yield result
        }
      }

    private def hasProgress(issue: Issue, sinceDays: Int): Task[Boolean] = {
      for {
        now <- Clock.instant
        progressLimit = now
          .atZone(ZoneId.systemDefault())
          .minus(sinceDays, DAYS)
          .`with`(LocalTime.MIDNIGHT)
        (changelogs, comments) <- client
          .getChangelogs(issue.key)
          .zipPar(client.getComments(issue.key))
      } yield {
        val hasNewChanges = changelogs.exists {
          case Changelog(_, _, created, items, _) =>
            created.isAfter(progressLimit) &&
            !items.forall { item =>
              // changes we don't count as progress
              item.field == "Rank"
              || item.field == "labels"
            }
        }

        val hasNewComments = comments.exists {
          case Comment(_, _, _, _, created, updated, _, _) =>
            created.isAfter(progressLimit)
            || updated.isAfter(progressLimit)
        }

        hasNewChanges || hasNewComments
      }
    }

    private def anyDescendantHasProgress(
        issue: Issue,
        sinceDays: Int
    ): Task[Boolean] = {
      def checkProgress(issue: Issue) =
        hasProgress(issue, sinceDays).filterOrFail(_.self)(NoProgress)
      ZIO.logSpan("anyDescendantHasProgress") {
        for {
          descendants <- client.getDescendants(issue.key)
          progress = descendants match {
            case Nil          => ZIO.succeed(false)
            case head :: next =>
              checkProgress(head).raceAll(next.map(checkProgress))
          }
          result <- progress.catchSome { case NoProgress =>
            ZIO.succeed(false)
          }
        } yield {
          result
        }
      }
    }

    /** A set of issues, considered together, is determined to be "In Progress"
      * if some of them are In Progress or some are Done while there are others
      * still not Done.
      *
      * @param issues
      * @return
      */
    private def aggregateInProgress(issues: List[Issue]) = {
      val someInProgress = issues.exists(_.inProgress)
      val partialDone = issues.exists(_.isDone) && issues.exists(!_.isDone)
      someInProgress || partialDone
    }

    override def stale(source: File, target: Option[File]): Task[File] =
      ZIO.logSpan("stale") {
        extract(source, target.getOrElse(sibling(source, ".stale"))) { key =>
          for {
            issue <- client.getIssue(key)
            result <-
              if (issue.isDone) {
                ZIO.succeed(false)
              } else if (issue.inProgress) {
                (
                  hasProgress(issue, StaleProgressDays)
                    || anyDescendantHasProgress(issue, StaleProgressDays)
                ).negate
              } else {
                allDescendantsStale(issue, StaleProgressDays)
              }
          } yield result
        }
      }

    def allDescendantsStale(issue: Issue, sinceDays: Int): Task[Boolean] =
      ZIO.logSpan("allDescendantsStale") {
        for {
          descendants <- client.getDescendants(issue.key)
          inProgress = aggregateInProgress(descendants)
          hasProgress <- anyDescendantHasProgress(issue, sinceDays)
        } yield {
          inProgress && !hasProgress
        }
      }

    /** Remove empty sections and extra blank lines.
      *
      * @param lines
      *   output lines
      * @return
      *   pruned output
      */
    def prune(lines: List[String]): List[String] = {

      def level(section: String) = section.takeWhile(_ == '#').length

      /** Prune a section
        *
        * @return
        *   tuple of (pruned section, unprocessed lines)
        */
      def pruneSection(
          start: String,
          in: List[String],
          entries: List[String]
      ): (List[String], List[String]) = {
        in match {
          case Nil =>
            entries match { // this is the last section
              case Nil                                     => (Nil, Nil)
              case nonEmpty if nonEmpty.exists(_.nonEmpty) =>
                (nonEmpty :+ start, Nil)
              case _ => (Nil, Nil) // entries are just blanks
            }
          case line :: rest =>
            line.trim match {
              case "" =>
                entries match {
                  case "" :: _ =>
                    // ignore consecutive blank lines
                    pruneSection(
                      start,
                      rest,
                      entries
                    )
                  case _ =>
                    // keep the first blank in
                    pruneSection(start, rest, "" :: entries)
                }
              case section if section.startsWith("#") =>
                if (level(section) > level(start)) {
                  // entering subsection
                  val (subsection, remaining) = pruneSection(section, rest, Nil)
                  pruneSection(start, remaining, subsection ++ entries)
                } else {
                  // end of section
                  entries match {
                    case Nil => (Nil, section :: rest)
                    case nonEmpty if nonEmpty.exists(_.nonEmpty) =>
                      (nonEmpty :+ start, section :: rest)
                    case _ => (Nil, section :: rest) // entries are just blanks
                  }
                }
              case _ => pruneSection(start, rest, line :: entries)
            }
        }
      }

      def doPrune(in: List[String], out: List[String]): List[String] = {
        in match {
          case Nil          => out
          case line :: rest =>
            line.trim match {
              case "" =>
                out match {
                  case Nil => doPrune(rest, out) // ignore leading blank lines
                  case "" :: _ =>
                    doPrune(rest, out) // ignore consecutive blanks
                  case _ => doPrune(rest, "" :: out) // keep first blank
                }
              case section if section.startsWith("#") =>
                val (subsection, remaining) = pruneSection(section, rest, Nil)
                doPrune(remaining, subsection ++ out)
              case nonblank => doPrune(rest, nonblank :: out)
            }
        }
      }

      doPrune(lines, Nil).reverse
    }
  }

  private case object NoProgress extends Exception with NoStackTrace

  def layer() = ZLayer {
    for {
      client <- ZIO.service[Client]
    } yield LiveImpl(client): Inspector
  }
}
