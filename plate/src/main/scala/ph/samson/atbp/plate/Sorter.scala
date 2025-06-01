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
            case None    => Nil
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
          val issues = sourceAtLevel(level).filterNot(_.isDone)
          val targetOrder = issues.map(_.key).distinct

          val levelSort = ZIO.logSpan(s"doSort $level") {
            for {
              _ <- ZIO.attempt(
                require(
                  issues.forall(_.fields.issuetype.hierarchyLevel == level)
                )
              )
              currentOrder <- client.getIssuesRanked(targetOrder)

              _ <- ZIO.logInfo(
                s"${targetOrder.length} to be sorted at level $level"
              )
              count <- reorder(currentOrder.map(_.key), targetOrder)
              _ <- ZIO.logInfo(s"sorted level $level in $count rerankings")
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

    /** Rerank Jira items in the current list according to the order given by
      * the target list.
      *
      * Both lists must contain exactly the same elements, in possibly different
      * orders.
      *
      * @param current
      *   keys ordered in the current Jira ranking
      * @param target
      *   keys ordered in the desired ranking
      * @return
      *   number of rerankings executed
      */
    def reorder(current: List[String], target: List[String]): Task[Int] = {

      /** Actual work happens here.
        *
        * We process the current key order and target key order in a series of
        * recursive steps and determine the list of rerankings to be executed on
        * the current key order to arrive at the target key order.
        *
        * Let's work through an example.
        *
        * Step 0. Initial state.
        *   - c = [A, B, C, D, E]
        *   - t = [A, D, C, B, E]
        *   - a = []
        *   - r = []
        *
        * Step 1. Order is correct. Nothing to do to A. Proceed with next item.
        *   - c = [B, C, D, E]
        *   - t = [D, C, B, E]
        *   - a = []
        *   - r = []
        *
        * Step 2. Put current head, B, aside as we look for the match to target
        * head, D.
        *   - c = [C, D, E]
        *   - t = [D, C, B, E]
        *   - a = [B]
        *   - r = []
        *
        * Step 3. Still no match, put C aside.
        *   - c = [D, E]
        *   - t = [D, C, B, E]
        *   - a = [C, B]
        *   - r = []
        *
        * Step 4. Match found. D goes before everything we've put aside. Because
        * B is the top of the current order, we rerank D > B. Restore the items
        * we've put aside back to the current order to be processed
        *   - c = [B, C, E]
        *   - t = [C, B, E]
        *   - a = []
        *   - r = [D > B]
        *
        * Step 5. Put B aside again.
        *   - c = [C, E]
        *   - t = [C, B, E]
        *   - a = [B]
        *   - r = [D > B]
        *
        * Step 6. Match found. C goes before B.
        *   - c = [B, E]
        *   - t = [B, E]
        *   - a = []
        *   - r = [C > B, D > B]
        *
        * Steps 7 and 8 find the remaining items to be in the correct order.
        *
        * Step 9. The result is the reverse of the accumulated reranks.
        *   - return [D > B, C > B]
        *
        * @param curRemaining
        *   where we are in the current list for example, we start with
        * @param tarRemaining
        *   where we are in the target list for example, we start with
        * @param aside
        *   items in the current list we have set aside to be reranked
        * @param reranks
        *   rerankings to be done
        * @return
        *   rerankings to be executed
        */
      @tailrec
      def computeReranks(
          curRemaining: List[String],
          tarRemaining: List[String],
          aside: List[String],
          reranks: List[Rerank]
      ): Task[List[Rerank]] = {
        curRemaining match {
          case Nil =>
            tarRemaining match {
              case Nil =>
                // We have finished both lists.
                //
                // This is Step 9 in the example.
                ZIO.succeed(reranks.reverse)
              case tarHead :: tarNext =>
                // We don't expect this to happen.
                ZIO.fail(
                  new IllegalStateException(
                    s"current list finished while target at $tarHead :: $tarNext"
                  )
                )
            }

          case curHead :: curNext =>
            tarRemaining match {
              case Nil =>
                // We don't expect this to happen.
                ZIO.fail(
                  new IllegalStateException(
                    s"target list finished while current at $curHead :: $curNext"
                  )
                )
              case tarHead :: tarNext =>
                if (curHead == tarHead) {
                  if (aside.isEmpty) {
                    // Order is correct, proceed with next item.
                    //
                    // This happens in steps 1, 7, and 8 in the example.
                    computeReranks(curNext, tarNext, Nil, reranks)
                  } else {
                    // curHead should go next in the order before everything
                    // that we've put aside so far.
                    // The rerank is only against the first thing we've set
                    // aside (restore.head) because we don't know if the other
                    // items need to be reranked among themselves.
                    //
                    // This happens in steps 4 and 6 in the example.
                    val restore = aside.reverse
                    val rerank = Rerank(curHead, restore.head)
                    computeReranks(
                      restore ++ curNext,
                      tarNext,
                      Nil,
                      rerank :: reranks
                    )
                  }
                } else {
                  // Put current head aside and find next match to target.
                  //
                  // This happens in steps 2, 3, and 5 in the example.
                  computeReranks(
                    curNext,
                    tarRemaining,
                    curHead :: aside,
                    reranks
                  )
                }
            }
        }
      }

      def executeReranks(reranks: List[Rerank]): Task[Int] = {
        // Rerankings before the same key can be done in one call
        val lowKeys = reranks.map(_.lowKey).distinct
        val grouped = for {
          lowKey <- lowKeys
        } yield {
          lowKey -> reranks.filter(_.lowKey == lowKey).map(_.highKey)
        }
        val execs = ZIO.foreach(grouped) { case (lowKey, highKeys) =>
          ZIO.logDebug(
            s"execute rerank $highKeys before $lowKey"
          ) *> (if (highKeys.length <= 50) {
                  client.rankIssuesBefore(highKeys, lowKey, None).as(1)
                } else {
                  rerank((highKeys :+ lowKey).grouped(51).toList)
                })
        }

        execs.map(_.sum)
      }

      for {
        _ <- ZIO.attempt {
          require(
            current.length == target.length,
            s"current (${current.length}) length not equal to target (${target.length})"
          )
          require(
            current.forall(target.contains),
            s"current not in target: ${current.filterNot(target.contains)}"
          )
          require(
            target.forall(current.contains),
            s"target not in current: ${target.filterNot(current.contains)}"
          )
        }
        _ <- ZIO.logDebug(
          s"reordering ((current, target), position)\n${current.zip(target).zipWithIndex.mkString("\n")}"
        )
        reranks <- computeReranks(current, target, Nil, Nil)
        _ <- ZIO.logDebug(s"reranks: ${reranks.mkString("\n")}")
        count <- executeReranks(reranks)
      } yield count
    }

    def rerank(groups: List[List[String]]): Task[Int] = {
      def rerankOne(
          group: List[String],
          beforeKey: Option[String]
      ): Task[Option[String]] = {
        group.reverse match {
          case Nil              => ZIO.none
          case last :: previous =>
            for {
              _ <- beforeKey match {
                case None         => ZIO.unit
                case Some(before) =>
                  ZIO.logDebug(
                    s"rerankOne $last before $before"
                  ) *> client.rankIssuesBefore(List(last), before, None)
              }
              _ <- previous match {
                case Nil      => ZIO.unit
                case nonEmpty =>
                  val top = nonEmpty.reverse
                  ZIO.logDebug(
                    s"rerankOne ${top.head} + ${top.tail.length} before $last"
                  ) *> client.rankIssuesBefore(top, last, None)
              }
            } yield group.headOption
        }
      }

      def doRerank(
          toRank: List[List[String]],
          lastKey: Option[String],
          count: Int
      ): Task[Int] = {
        toRank match {
          case Nil              => ZIO.succeed(count)
          case last :: previous =>
            for {
              lastTop <- rerankOne(last, lastKey)
              c <- doRerank(previous, lastTop, count + 1)
            } yield c
        }
      }

      doRerank(groups.reverse, None, 0)
    }
  }

  def layer() = ZLayer {
    for {
      client <- ZIO.service[Client]
    } yield LiveImpl(client): Sorter
  }

  /** Command to rank highKey before lowKey. That is, highKey > lowKey.
    */
  private case class Rerank(highKey: String, lowKey: String)
}
