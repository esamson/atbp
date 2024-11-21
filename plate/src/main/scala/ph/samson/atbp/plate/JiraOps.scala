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
      keys match
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

    def getChildren(keys: List[String]): Task[List[Issue]] = {
      keys match
        case Nil => ZIO.succeed(Nil)
        case parents =>
          ZIO.logSpan("getChildren") {
            for {
              children <- client.search(childrenJql(parents))
            } yield children
          }
    }
  }

  extension (issue: Issue) {
    def isDone: Boolean = issue.fields.status.statusCategory.name == "Done"
  }

  private def childrenJql(keys: List[String]) =
    s"parent IN (${keys.mkString(",")})"
}
