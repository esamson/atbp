package ph.samson.atbp.plate

import ph.samson.atbp.jira.Client
import ph.samson.atbp.jira.model.Issue
import zio.Task
import zio.ZIO

object JiraOps {
  extension (client: Client) {

    def getDescendants(key: String*): Task[List[Issue]] = getDescendants(
      key.toList
    )

    def getDescendants(keys: List[String]): Task[List[Issue]] = {
      keys match {
        case Nil => ZIO.succeed(Nil)
        case parents =>
          ZIO.logSpan("getDescendants") {
            for {
              children <- getChildren(parents)
              childKeys = children.map(_.key)
              descendants <- getDescendants(childKeys)
            } yield {
              children ++ descendants
            }
          }
      }
    }

    def getChildren(keys: List[String]): Task[List[Issue]] = {
      keys match {
        case Nil => ZIO.succeed(Nil)
        case parents =>
          ZIO.logSpan("getChildren") {
            for {
              children <- client.search(childrenJql(parents))
            } yield children
          }
      }
    }
  }

  extension (issue: Issue) {
    def inProgress: Boolean =
      issue.fields.status.statusCategory.name == "In Progress"

    def isDone: Boolean = issue.fields.status.statusCategory.name == "Done"

    def projectKey: String = issue.key.substring(0, issue.key.indexOf('-'))

    def webUrl = {
      val base = issue.self.substring(
        0,
        issue.self.indexOf('/', "https://".length)
      )
      s"$base/browse/${issue.key}"
    }
  }

  private def childrenJql(keys: List[String]) =
    s"parent IN (${keys.mkString(",")})"
}
