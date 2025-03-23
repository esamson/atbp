package ph.samson.atbp.plate

import better.files.File
import ph.samson.atbp.jira.Client
import zio.Task
import zio.ZIO
import zio.ZLayer
import JiraOps.*
import ph.samson.atbp.jira.model.Issue

trait Sorter {
  def sort(source: File): Task[Unit]
}

object Sorter {

  private val JiraKey = """\(https://.*/browse/([A-Z]+-\d+)\)""".r

  private class LiveImpl(client: Client) extends Sorter {
    override def sort(source: File): Task[Unit] = ZIO.logSpan("sort") {
      for {
        content <- ZIO.attemptBlockingIO(source.contentAsString)
        sourceKeys = JiraKey.findAllMatchIn(content).map(_.group(1)).toList
        _ <- ZIO.logInfo(
          s"keys: ${sourceKeys.length}, ${sourceKeys.distinct.length}"
        )
        (sourceIssues, ancestors) <-
          client.getIssues(sourceKeys) <&>
            client.getAncestors(sourceKeys)

        _ <- sort(sourceKeys, sourceIssues, ancestors)

      } yield ()
    }

    private def sort(sourceKeys: List[String], sourceIssues: List[Issue],
                     ancestors: List[Issue]): Task[Unit] = {
      val levels = sourceIssues.map(_.fields.issuetype.hierarchyLevel).distinct
      val lookup = (sourceIssues ++ ancestors).map(i => i.key -> i).toMap

      def atLevel( level: Int)(issue: Issue): Task[List[Issue]] = {
        if (issue.fields.issuetype.hierarchyLevel < level) {
          issue.fields.parent match {
            case None => ZIO.succeed(Nil)
            case Some(p) => for {
              parent <- client.getIssue(p.key)
              leveled <- atLevel( level)(parent)
            } yield {
              leveled
            }
          }
        } else if (issue.fields.issuetype.hierarchyLevel > level) {
          for {
            children <- client.getChildren(issue.key)
            leveled <- ZIO.foreachPar(children)(atLevel(level))
          } yield {
            leveled.flatten
          }
        } else {
          ZIO.succeed(List(issue))
        }
      }

      def sourceAtLevel(level: Int): Task[List[Issue]] = ZIO.logSpan(s"sourceAtLevel $level") {
        for {
          issues <- ZIO.foreachPar(sourceKeys.map(lookup))(atLevel(level))
        } yield {
          issues.flatten
        }
      }

      def doSort(level: Int, sorts: List[Task[Unit]]): List[Task[Unit]] = {
        if (level <= levels.max) {
          val levelSort = for {
            issues <- sourceAtLevel(level)
            levelKeys = issues.map(_.key)
            groups = levelKeys.distinct
              .grouped(51)
              .toList // 50 issues to sort + 1 reference issue
            _ <- rerank(groups)
          } yield ()
          doSort(level + 1, levelSort :: sorts)
        } else {
          sorts
        }
      }

      ZIO.collectAllParDiscard( doSort(levels.min, Nil) )
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
