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
     * @param current keys ordered in the current Jira ranking
     * @param target keys ordered in the desired ranking
     * @return number of rerankings executed
     */
    def reorder(current: List[String], target: List[String]): Task[Int] = {

      /** Actual work happens here.
       *
       * We process the current key order and target key order in a series of
       * recursive steps and determine the list of rerankings to be executed
       * on the current key order to arrive at the target key order.
       *
       * Let's work through an example.
       *
       * Step 0. Initial state.
       * c = [A, B, C, D, E]
       * t = [A, D, C, B, E]
       * a = []
       * r = []
       *
       * Step 1. Order is correct. Nothing to do to A. Proceed with next item.
       * c = [B, C, D, E]
       * t = [D, C, B, E]
       * a = []
       * r = []
       *
       * Step 2. Put current head, B, aside as we look for the match to target
       * head, D.
       * c = [C, D, E]
       * t = [D, C, B, E]
       * a = [B]
       * r = []
       *
       * Step 3. Still no match, put C aside.
       * c = [D, E]
       * t = [D, C, B, E]
       * a = [C, B]
       * r = []
       *
       * Step 4. Match found. D goes before everything we've put aside. Because
       * B is the top of the current order, we rerank D > B. Restore the items
       * we've put aside back to the current order to be processed
       * c = [B, C, E]
       * t = [C, B, E]
       * a = []
       * r = [D > B]
       *
       * Step 5. Put B aside again.
       * c = [C, E]
       * t = [C, B, E]
       * a = [B]
       * r = [D > B]
       *
       * Step 6. Match found. C goes before B.
       * c = [B, E]
       * t = [B, E]
       * a = []
       * r = [C > B, D > B]
       *
       * Steps 7 and 8 find the remaining items to be in the correct order.
       * The result is the reverse of the accumulated reranks.
       * return [D > B, C > B]
       *
       * @param curRemaining where we are in the current list
       *                     for example, we start with
       * @param tarRemaining where we are in the target list
       *                     for example, we start with
       * @param aside items in the current list we have set aside to be reranked
       * @param reranks rerankings to be done
       * @return rerankings to be executed
       */
      def doRerank(curRemaining: List[String], tarRemaining: List[String], aside: List[String], reranks: List[Rerank]): Task[List[Rerank]] = {
        curRemaining match {
          case Nil => tarRemaining match {
            case Nil =>
              // Let's get the simple case out of the way.
              // We have finished both lists.
              ZIO.succeed(reranks.reverse)
            case tarHead :: tarNext =>
              // I don't know if I expect this to happen.
              ZIO.fail(new IllegalStateException(s"current list finished while target at $tarHead :: $tarNext"))
          }
            
          case curHead :: curNext => tarRemaining match {
            case Nil =>
              // I don't know if I expect this to happen.
              ZIO.fail(new IllegalStateException(s"target list finished while current at $curHead :: $curNext"))
            case tarHead :: tarNext =>
              if (curHead == tarHead) {
                if (aside.isEmpty) {
                  // Order is correct, proceed with next item.
                  //
                  // For example, we get this on recursion 1:
                  // c = [B, C, D, E]
                  // t = [D, B, C, E]
                  doRerank(curNext, tarNext, Nil, reranks)
                } else {
                  // curHead should go next in the order.
                  // Everything in aside should be ranked after it.
                  //
                  // In recursion 4 we restore the keys we set aside.
                  // Reversed because we accumulated them that way.
                  //
                  // We now know that current head should be ranked before the
                  // restored keys.
                  //
                  // c = [B, C, E]
                  // t = [B, C, E]
                  // a = []
                  // r = [D > B]
                  val restore = aside.reverse
                  val rerank = Rerank(curHead, restore.head)
                  doRerank(restore ++ curNext, tarNext, Nil, rerank :: reranks)
                }
              } else {
                // Put current head aside and find next match to target.
                //
                // We get this in recursion 2:
                // c = [C, D, E]
                // t = [D, B, C, E]
                // a = [B]
                //
                // Then recursion 3:
                // c = [D, E]
                // t = [D, B, C, E]
                // a = [C, B]
                doRerank(curNext, tarRemaining, curHead :: aside, reranks)
              }
          }
        }
      }

      def executeReranks(reranks: List[Rerank]): Task[Int] = {
        // Rerankings before the same key can be done in one call
        val groups = reranks.groupBy(_.lowKey).view.mapValues(_.map(_.highKey))
        val execs = ZIO.foreachParDiscard(groups) {
          case (lowKey, highKeys) =>
            ZIO.logInfo(s"execute rerank $highKeys before $lowKey") *> (if (highKeys.length <= 50) {
                client.rankIssuesBefore(highKeys, lowKey, None)
              } else {
                rerank((highKeys :+ lowKey).grouped(51).toList)
              })
        }

        execs.as(groups.size)
      }

      for {
        _ <- ZIO.attempt {
          require(current.length == target.length, s"current (${current.length}) length not equal to target (${target.length})")
          require(current.forall(target.contains), s"current not in target: ${current.filterNot(target.contains)}")
          require(target.forall(current.contains), s"target not in current: ${target.filterNot(current.contains)}")
        }
        _ <- ZIO.logInfo(s"reordering\n${current.zip(target).zipWithIndex.mkString("\n")}")
        reranks <- doRerank(current, target, Nil, Nil)
        count <- executeReranks(reranks)
      } yield count
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
              s"rerankOne ${group.head} + ${group.tail.length} before $beforeKey"
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

  /** Command to rank highKey before lowKey.
   * That is, highKey > lowKey.
   */
  private case class Rerank(highKey: String, lowKey: String)
}
