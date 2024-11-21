package ph.samson.atbp.plate

import better.files.File
import ph.samson.atbp.jira.Client
import ph.samson.atbp.plate.JiraOps.getDescendants
import zio.Task
import zio.ZIO
import zio.ZLayer

trait Labeler {
  def label(source: File, value: String): Task[Int]
}

object Labeler {

  private val JiraKey = """\(https://.*/browse/([A-Z]+-\d+)\)""".r

  private class LiveImpl(client: Client) extends Labeler {
    override def label(source: File, value: String): Task[Int] =
      ZIO.logSpan(s"label $value") {
        for {
          searchLabeled <- client.search(s"labels = $value").fork

          content <- ZIO.attemptBlockingIO(source.contentAsString)
          sourceKeys = JiraKey.findAllMatchIn(content).map(_.group(1)).toList
          descendants <- client.getDescendants(sourceKeys)
          localKeys = sourceKeys ++ descendants.map(_.key)

          alreadyLabeled <- searchLabeled.join
          currentKeys = alreadyLabeled.map(_.key)

          toAdd = localKeys.filterNot(currentKeys.contains)
          _ <- ZIO
            .logInfo(s"toAdd ${toAdd.length}: $toAdd")
            .when(toAdd.nonEmpty)
          add <- ZIO
            .foreachParDiscard(toAdd)(key => client.addLabel(key, value))
            .fork

          toRemove = currentKeys.filterNot(localKeys.contains)
          _ <- ZIO
            .logInfo(s"toRemove ${toRemove.length}: $toRemove")
            .when(toRemove.nonEmpty)
          _ <- ZIO.foreachParDiscard(toRemove)(key =>
            client.removeLabel(key, value)
          )

          _ <- add.join
        } yield 1
      }

  }

  def layer() = ZLayer {
    for {
      client <- ZIO.service[Client]
    } yield LiveImpl(client): Labeler
  }
}
