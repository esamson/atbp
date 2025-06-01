package ph.samson.atbp.plate

import better.files.File
import ph.samson.atbp.jira.Client
import ph.samson.atbp.jira.model.Issue
import ph.samson.atbp.plate.JiraOps.*
import zio.Clock
import zio.Task
import zio.ZIO
import zio.ZLayer

import java.time.LocalTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit.DAYS

trait Inspector {
  def cooking(source: File): Task[File]
  def stale(source: File): Task[File]
  def done(source: File): Task[File]
}

object Inspector {

  val JiraLink = """.*\(https://.*/browse/([A-Z]+-\d+)\).*""".r

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
    private def extract(source: File, suffix: String)(
        check: String => Task[Boolean]
    ): Task[File] =
      for {
        sourceLines <- ZIO.attemptBlockingIO(source.lines.toList)
        targetLines <- ZIO.filterPar(sourceLines)(includeLine(check))
        outFile <- ZIO.attemptBlockingIO {
          val name = source.`extension`() match {
            case Some(ext) =>
              source.nameWithoutExtension(includeAll = false) + suffix + ext
            case None => source.name + suffix
          }
          val out = source.sibling(name)
          out.overwrite(prune(targetLines).mkString("\n"))
        }
      } yield outFile

    override def done(source: File): Task[File] = ZIO.logSpan("done") {
      extract(source, ".done") { key =>
        for {
          issue <- client.getIssue(key)
          descendants <- client.getDescendants(key)
        } yield issue.isDone && descendants.forall(_.isDone)
      }
    }

    override def cooking(source: File): Task[File] =
      ZIO.logSpan("cooking") {
        extract(source, ".cooking") { key =>
          for {
            issue <- client.getIssue(key)
            result <-
              if (issue.isDone) {
                ZIO.succeed(false)
              } else if (issue.inProgress) {
                ZIO.succeed(true)
              } else {
                anyDescendantInProgress(key)
              }
          } yield result
        }
      }

    def anyDescendantInProgress(key: String): Task[Boolean] =
      ZIO.logSpan("anyDescendantInProgress") {
        for {
          descendants <- client.getDescendants(key)
        } yield {
          aggregateInProgress(descendants)
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

    override def stale(source: File): Task[File] =
      ZIO.logSpan("stale") {
        extract(source, ".stale") { key =>
          for {
            now <- Clock.instant
            freshLimit = now
              .atZone(ZoneId.systemDefault())
              .minus(28, DAYS)
              .`with`(LocalTime.MIDNIGHT)
            issue <- client.getIssue(key)
            result <-
              if (issue.isDone) {
                ZIO.succeed(false)
              } else if (
                issue.inProgress &&
                issue.fields.updated.isAfter(freshLimit)
              ) {
                ZIO.succeed(false)
              } else {
                allDescendantsStale(key, freshLimit)
              }
          } yield result
        }
      }

    def allDescendantsStale(
        key: String,
        freshLimit: ZonedDateTime
    ): Task[Boolean] =
      ZIO.logSpan("allDescendantsStale") {
        for {
          descendants <- client.getDescendants(key)
          inProgress = aggregateInProgress(descendants)
        } yield {
          inProgress &&
          descendants
            .filterNot(_.isDone)
            .forall(_.fields.updated.isBefore(freshLimit))
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

  def layer() = ZLayer {
    for {
      client <- ZIO.service[Client]
    } yield LiveImpl(client): Inspector
  }
}
