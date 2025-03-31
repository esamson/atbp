package ph.samson.atbp.plate

import better.files.File
import ph.samson.atbp.jira.Client
import ph.samson.atbp.jira.model.Issue
import zio.Task
import zio.ZIO
import zio.ZLayer

import scala.annotation.tailrec

import JiraOps.*

trait Sorter {

  /** Rank Jira tickets according to their order in the given source file. */
  def sort(source: File): Task[Unit]
}

object Sorter {

  private val JiraKey = """\(https://.*/browse/([A-Z]+-\d+)\)""".r

  private class LiveImpl(client: Client) extends Sorter {
    override def sort(source: File): Task[Unit] = ZIO.logSpan("sort") {
      for {
        content <- ZIO.attemptBlockingIO(source.contentAsString)
        sourceKeys = JiraKey
          .findAllMatchIn(content)
          .map(_.group(1))
          .toList
          .distinct
        _ <- ZIO.logInfo(s"Sorting ${sourceKeys.length} keys from $source")
        (sourceIssues, ancestors, descendants) <-
          client.getIssues(sourceKeys) <&>
            client.getAncestors(sourceKeys) <&>
            client.getDescendants(sourceKeys)

        _ <- sort(sourceKeys, sourceIssues, ancestors, descendants)

      } yield ()
    }

    private def sort(
        sourceKeys: List[String],
        sourceIssues: List[Issue],
        ancestors: List[Issue],
        descendants: List[Issue]
    ): Task[Unit] = {
      val levels = sourceIssues.map(_.fields.issuetype.hierarchyLevel).distinct
      val lookup =
        (sourceIssues ++ ancestors ++ descendants).map(i => i.key -> i).toMap

      def getChildren(issue: Issue) =
        descendants.filter(_.fields.parent.exists(_.key == issue.key))

      /** Get the level equivalent of the given issue.
        *
        * Issue level is according to the issue type. For example, an epic is
        * one level higher than a story.
        *
        * @param issue
        *   source issue
        * @param level
        *   target level
        * @return
        *   If issue is already at the target level, return that issue. If it is
        *   below the target level, find the ancestor at the target level. If it
        *   is above the target level, list all descendants at the target level.
        *   If there are no appropriate ancestors or descendants, return the
        *   empty list.
        */
      def atLevel(issue: Issue, level: Int): List[Issue] = {
        val issueLevel = issue.fields.issuetype.hierarchyLevel
        if (issueLevel < level) {
          issue.fields.parent match {
            case None => Nil
            case Some(p) =>
              val parent = lookup(p.key)
              // limit to only one level up
              // otherwise, we'd loop infinitely below and above the target level
              if (parent.fields.issuetype.hierarchyLevel == issueLevel + 1) {
                atLevel(parent, level)
              } else {
                Nil
              }
          }
        } else if (issueLevel > level) {
          for {
            child <- getChildren(issue)
            // limit to only one level down
            // otherwise, we'd loop infinitely below and above the target level
            if child.fields.issuetype.hierarchyLevel == issueLevel - 1
            leveled <- atLevel(child, level)
          } yield {
            leveled
          }
        } else {
          List(issue)
        }
      }

      def sourceAtLevel(level: Int): List[Issue] =
        for {
          issue <- sourceKeys.map(lookup)
          leveled <- atLevel(issue, level)
        } yield {
          leveled
        }

      @tailrec
      def doSort(level: Int, sorts: List[Task[Unit]]): List[Task[Unit]] = {
        if (level <= levels.max) {
          val issues = sourceAtLevel(level)
          val levelKeys = issues.map(_.key).distinct
          val groups = levelKeys
            .grouped(51)
            .toList // 50 issues to sort + 1 reference issue
          val levelSort = ZIO.logSpan(s"doSort $level") {
            for {
              _ <- ZIO.attempt(
                require(
                  issues.forall(_.fields.issuetype.hierarchyLevel == level)
                )
              )
              _ <- ZIO.logInfo(
                s"${levelKeys.length} to be sorted at level $level"
              )
              _ <- rerank(groups)
              _ <- ZIO.logInfo(s"sorted level $level")
            } yield ()
          }
          doSort(level + 1, levelSort :: sorts)
        } else {
          sorts
        }
      }

      ZIO.logSpan("sort") {
        ZIO.collectAllParDiscard(doSort(levels.min, Nil))
      }
    }

    def rerank(groups: List[List[String]]): Task[Unit] = {
      def rerankOne(
          group: List[String],
          beforeKey: Option[String]
      ): Task[Option[String]] = {
        group.reverse match {
          case Nil => ZIO.none
          case last :: previous =>
            ZIO.logDebug(
              s"rerank ${group.head} + ${group.tail.length} before $beforeKey"
            ) *> {
              for {
                _ <- beforeKey match {
                  case None => ZIO.unit
                  case Some(before) =>
                    client.rankIssuesBefore(List(last), before, None)
                }
                _ <- previous match {
                  case Nil => ZIO.unit
                  case nonEmpty =>
                    val top = nonEmpty.reverse
                    client.rankIssuesBefore(top, last, None)
                }
              } yield group.headOption
            }
        }
      }

      def doRerank(
          toRank: List[List[String]],
          lastKey: Option[String]
      ): Task[Unit] = {
        toRank match {
          case Nil => ZIO.unit
          case last :: previous =>
            for {
              lastTop <- rerankOne(last, lastKey)
              _ <- doRerank(previous, lastTop)
            } yield ()
        }
      }

      doRerank(groups.reverse, None)
    }
  }

  def layer() = ZLayer {
    for {
      client <- ZIO.service[Client]
    } yield LiveImpl(client): Sorter
  }
}
