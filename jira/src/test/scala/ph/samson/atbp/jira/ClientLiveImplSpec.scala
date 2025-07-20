package ph.samson.atbp.jira

import zio.System
import zio.ZIO
import zio.test.*

import TestAspect.*

object ClientLiveImplSpec extends JiraSpec {

  override def spec = suite("Jira Client")(
    test("search") {
      val search = for {
        client <- ZIO.service[Client]
        query <- System.envOrElse(
          EnvVars.Search,
          "statusCategory != Done OR statusCategoryChangedDate >= -14d"
        )
        result <- client.search(query)
        _ <- ZIO.log(s"result\n${pprint(result)}")
      } yield result

      for (s <- Live.live(search).exit) yield assertTrue(s.isSuccess)
    },
    test("rankIssues") {
      val rank = for {
        client <- ZIO.service[Client]
        result <- client.rankIssuesBefore(
          List("AAT-146", "AAT-155", "AAT-41"),
          "AAT-212",
          None
        )
        _ <- ZIO.log(s"result\n${pprint(result)}")
      } yield result

      for (s <- Live.live(rank).exit) yield assertTrue(s.isSuccess)
    },
    test("getIssue") {
      val rank = for {
        client <- ZIO.service[Client]
        result <- client.getIssue("CHRONOS-10393")
        _ <- ZIO.log(s"result\n${pprint(result)}")
      } yield result

      for (s <- Live.live(rank).exit) yield assertTrue(s.isSuccess)
    }
  ) @@ ifEnv(EnvVars.Site)(!_.isBlank)
    @@ ifEnv(EnvVars.User)(!_.isBlank)
    @@ ifEnv(EnvVars.Token)(!_.isBlank)
}
