package ph.samson.atbp.plate

import better.files.File
import ph.samson.atbp.jira.Client
import ph.samson.atbp.jira.model.Issue
import ph.samson.atbp.plate.Inspector.JiraLink
import zio.Task
import zio.ZIO
import zio.ZLayer

import scala.annotation.tailrec

import JiraOps.*

trait RadarScanner {

  def scan(source: File, excludeProjects: List[String]): Task[File]
}

object RadarScanner {

  private val Suffix = ".radar"

  private class LiveImpl(client: Client) extends RadarScanner {

    def scan(source: File, excludeProjects: List[String]): Task[File] =
      ZIO.logSpan("scan") {
        for {
          _ <- ZIO.logInfo(
            s"scan $source; excluding: ${excludeProjects.mkString(", ")}"
          )
          sourceLines <- ZIO.attemptBlockingIO(source.lines.toList)
          outputLines <- doScan(sourceLines, excludeProjects)
          outFile <- ZIO.attemptBlockingIO {
            val name = source.`extension`() match {
              case Some(ext) =>
                source.nameWithoutExtension(includeAll = false) + Suffix + ext
              case None => source.name + Suffix
            }
            val out = source.sibling(name)
            out.overwrite(outputLines.mkString("\n"))
          }
        } yield outFile
      }

    def doScan(
        sourceLines: List[String],
        excludeProjects: List[String]
    ): Task[List[String]] = {
      val plateKeys = sourceLines.collect { case JiraLink(key) =>
        key
      }
      val projects = plateKeys
        .map(key => key.substring(0, key.indexOf('-')))
        .toSet
        .filterNot(excludeProjects.contains)
        .toList

      for {
        _ <- ZIO.logInfo(s"projects:\n  ${projects.sorted.mkString("\n  ")}")

        (descendants, ancestors) <- client
          .getDescendants(plateKeys)
          .zipPar(
            client.getAncestors(plateKeys)
          )
        _ <- ZIO.logDebug(s"descendants: ${descendants.length}")
        _ <- ZIO.logDebug(s"ancestors: ${ancestors.length}")

        updated <- client.search(
          updatedJql(
            projects,
            plateKeys ++ descendants.map(_.key) ++ ancestors.map(_.key)
          )
        )
        _ <- ZIO.logDebug(s"updated: ${updated.length}")

        collapsed = collapse(updated)
        summary = collapsed.groupBy(_.projectKey).map { case (key, issues) =>
          s"$key -> ${issues.length}"
        }
        _ <- ZIO.logInfo(
          s"New items on the radar: ${collapsed.length}\n  ${summary.toList.sorted.mkString("\n  ")}"
        )
      } yield format(collapsed)
    }

  }

  private def format(issues: List[Issue]) = {

    def formatIssue(issue: Issue) =
      s"* [${issue.fields.summary}](${issue.webUrl})"

    def doFormat(
        remaining: List[Issue],
        project: String,
        result: List[String]
    ): List[String] = {
      remaining match {
        case Nil => result.reverse
        case head :: next =>
          if (head.projectKey == project) {
            doFormat(next, project, formatIssue(head) :: result)
          } else {
            if (result.isEmpty) {
              // Start of the document
              doFormat(
                remaining,
                head.projectKey,
                s"## ${head.projectKey}" :: "" :: result
              )
            } else {
              // Start of project section
              doFormat(
                remaining,
                head.projectKey,
                "" :: s"## ${head.projectKey}" :: "" :: result
              )
            }
          }
      }
    }

    doFormat(issues.sortBy(_.key), "", Nil)
  }

  /** Reduce list of issues to only the top-level items */
  private def collapse(issues: List[Issue]) = {
    val keys = issues.map(_.key).toSet

    @tailrec
    def doCollapse(remaining: List[Issue], result: List[Issue]): List[Issue] = {
      remaining match {
        case Nil => result
        case head :: next =>
          head.fields.parent match {
            case None => doCollapse(next, head :: result)
            case Some(parent) =>
              if (keys.contains(parent.key)) {
                doCollapse(next, result)
              } else {
                doCollapse(next, head :: result)
              }
          }
      }
    }

    doCollapse(issues, Nil)
  }

  private def updatedJql(projects: List[String], excludeKeys: List[String]) =
    s"""project IN (${projects.mkString(",")})
       |  AND statusCategory != Done
       |  AND updated >= -2w
       |  AND key NOT IN (${excludeKeys.mkString(",")})
       |""".stripMargin.trim

  def layer() = ZLayer {
    for {
      client <- ZIO.service[Client]
    } yield LiveImpl(client): RadarScanner
  }
}
