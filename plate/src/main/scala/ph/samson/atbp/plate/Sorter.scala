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

      def atLevel(level: Int)(issue: Issue): List[Issue] = {
        val issueLevel = issue.fields.issuetype.hierarchyLevel
        if (issueLevel < level) {
          issue.fields.parent match {
            case None => Nil
            case Some(p) =>
              val parent = lookup(p.key)
              // limit to only one level up
              // otherwise, we'd loop infinitely below and above the target level
              if (parent.fields.issuetype.hierarchyLevel == issueLevel + 1) {
                atLevel(level)(parent)
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
            leveled <- atLevel(level)(child)
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
          leveled <- atLevel(level)(issue)
        } yield {
          leveled
        }

      @tailrec
      def doSort(level: Int, sorts: List[Task[Unit]]): List[Task[Unit]] = {
        if (level <= levels.max) {
          val levelSort = ZIO.logSpan(s"doSort $level") {
            for {
              _ <- ZIO.logInfo(s"sorting at level $level")
              issues = sourceAtLevel(level)
              _ <- ZIO.attempt(
                require(
                  issues.forall(_.fields.issuetype.hierarchyLevel == level)
                )
              )
              _ <- ZIO.logInfo(
                s"${issues.length} to be sorted at level $level.\n${issues.mkString("\n")}"
              )
              levelKeys = issues.map(_.key)
              groups = levelKeys.distinct
                .grouped(51)
                .toList // 50 issues to sort + 1 reference issue
              _ <- ZIO.logInfo(s"reranking level $level")
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
