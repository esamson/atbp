package ph.samson.atbp.plate

import better.files.File
import ph.samson.atbp.jira.Client
import ph.samson.atbp.plate.JiraOps.*
import zio.Task
import zio.ZIO
import zio.ZLayer

import java.time.ZonedDateTime

trait Inspector {
  def done(source: File): Task[File]
  def cooking(source: File, since: ZonedDateTime): Task[File]
}

object Inspector {

  private val JiraLink = """.*\(https://.*/browse/([A-Z]+-\d+)\).*""".r

  private def includeLine(
      check: String => Task[Boolean]
  )(line: String): Task[Boolean] = {
    line match
      case ""                                 => ZIO.succeed(true)
      case heading if heading.startsWith("#") => ZIO.succeed(true)
      case JiraLink(key)                      => check(key)
      case _                                  => ZIO.succeed(false)
  }

  private class LiveImpl(client: Client) extends Inspector {

    private def extract(source: File, suffix: String)(
        check: String => Task[Boolean]
    ): Task[File] =
      for {
        sourceLines <- ZIO.attemptBlockingIO(source.lines.toList)
        targetLines <- ZIO.filterPar(sourceLines)(includeLine(check))
        outFile <- ZIO.attemptBlockingIO {
          val name = source.`extension`() match
            case Some(ext) =>
              source.nameWithoutExtension(includeAll = false) + suffix + ext
            case None => source.name + suffix
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

    override def cooking(source: File, since: ZonedDateTime): Task[File] =
      ZIO.logSpan("cooking") {
        extract(source, ".cooking") { key =>
          for {
            issue <- client.getIssue(key)
            result <-
              if issue.isDone then {
                ZIO.succeed(false)
              } else if issue.fields.statuscategorychangedate.isAfter(since)
              then {
                ZIO.succeed(true)
              } else {
                anyDescendantInProgress(key, since)
              }
          } yield result
        }
      }

    def anyDescendantInProgress(
        key: String,
        since: ZonedDateTime
    ): Task[Boolean] = ZIO.logSpan("anyDescendantInProgress") {
      for {
        descendants <- client.getDescendants(key)
      } yield descendants.exists(
        _.fields.statuscategorychangedate.isAfter(since)
      )
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
        in match
          case Nil =>
            entries match // this is the last section
              case Nil => (Nil, Nil)
              case nonEmpty if nonEmpty.exists(_.nonEmpty) =>
                (nonEmpty :+ start, Nil)
              case _ => (Nil, Nil) // entries are just blanks
          case line :: rest =>
            line.trim match
              case "" =>
                entries match
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
              case section if section.startsWith("#") =>
                if level(section) > level(start) then {
                  // entering subsection
                  val (subsection, remaining) = pruneSection(section, rest, Nil)
                  pruneSection(start, remaining, subsection ++ entries)
                } else {
                  // end of section
                  entries match
                    case Nil => (Nil, section :: rest)
                    case nonEmpty if nonEmpty.exists(_.nonEmpty) =>
                      (nonEmpty :+ start, section :: rest)
                    case _ => (Nil, section :: rest) // entries are just blanks
                }
              case nonBlank => pruneSection(start, rest, nonBlank :: entries)
      }

      def doPrune(in: List[String], out: List[String]): List[String] = {
        in match
          case Nil => out
          case line :: rest =>
            line.trim match
              case "" =>
                out match
                  case Nil => doPrune(rest, out) // ignore leading blank lines
                  case "" :: _ =>
                    doPrune(rest, out) // ignore consecutive blanks
                  case _ => doPrune(rest, "" :: out) // keep first blank
              case section if section.startsWith("#") =>
                val (subsection, remaining) = pruneSection(section, rest, Nil)
                doPrune(remaining, subsection ++ out)
              case nonblank => doPrune(rest, nonblank :: out)
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
