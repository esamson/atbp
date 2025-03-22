package ph.samson.atbp.plate

import better.files.File
import ph.samson.atbp.jira.Client
import zio.Task
import zio.ZIO
import zio.ZLayer

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
        groups = sourceKeys.distinct
          .grouped(51)
          .toList // 50 issues to sort + 1 reference issue
        _ <- ZIO.logInfo(
          s"grouped:\n ${groups.map(_.mkString("\n    ")).mkString("\n")}"
        )
        _ <- rerank(groups)
      } yield ()
    }

    def rerank(groups: List[List[String]]) = {
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
