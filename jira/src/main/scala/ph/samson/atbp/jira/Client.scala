package ph.samson.atbp.jira

import ph.samson.atbp.http.StatusCheck
import ph.samson.atbp.jira.model.Changelog
import ph.samson.atbp.jira.model.Comment
import ph.samson.atbp.jira.model.EditIssueRequest
import ph.samson.atbp.jira.model.Issue
import ph.samson.atbp.jira.model.PageBean
import ph.samson.atbp.jira.model.PageOfComments
import ph.samson.atbp.jira.model.RankIssuesRequest
import ph.samson.atbp.jira.model.SearchRequest
import ph.samson.atbp.jira.model.SearchResults
import zio.LogLevel
import zio.Scope
import zio.Task
import zio.ZIO
import zio.ZLayer
import zio.http
import zio.http.Body
import zio.http.Client as HttpClient
import zio.http.Header.Accept
import zio.http.Header.Authorization
import zio.http.Header.ContentType
import zio.http.Header.UserAgent
import zio.http.Header.UserAgent.ProductOrComment
import zio.http.Headers
import zio.http.MediaType
import zio.http.Status
import zio.http.URL
import zio.http.ZClient
import zio.http.ZClientAspect

import scala.annotation.tailrec

/** Jira REST API client.
  *
  * @see
  *   https://developer.atlassian.com/cloud/jira/platform/rest/
  * @see
  *   https://developer.atlassian.com/cloud/jira/software/rest/
  */
trait Client {
  def getIssue(key: String): Task[Issue]
  def getChangelogs(key: String): Task[List[Changelog]]
  def getComments(key: String): Task[List[Comment]]
  def search(jql: String): Task[List[Issue]]
  def addLabel(key: String, label: String): Task[Unit]
  def removeLabel(key: String, label: String): Task[Unit]
  def rankIssuesBefore(
      issues: List[String],
      reference: String,
      rankCustomFieldId: Option[Int]
  ): Task[Unit]
  def rankIssuesAfter(
      issues: List[String],
      reference: String,
      rankCustomFieldId: Option[Int]
  ): Task[Unit]
}

object Client {

  private case class PageRequest(
      startAt: Int,
      maxResults: Int,
      issueIdOrKey: String
  )

  private class LiveImpl(client: HttpClient) extends Client {

    val platformClient = client.addPath("/rest/api/3")
    val softwareClient = client.addPath("/rest/agile/1.0")

    override def getChangelogs(key: String): Task[List[Changelog]] = {
      def tail(head: PageBean[Changelog]) = {
        @tailrec
        def tailRequests(
            requests: List[PageRequest]
        ): List[PageRequest] = {
          val lastRequest = requests match {
            case Nil       => PageRequest(head.startAt, head.maxResults, key)
            case last :: _ => last
          }

          val nextStart = lastRequest.startAt + lastRequest.maxResults

          if (nextStart < head.total) {
            val nextRequest = lastRequest.copy(startAt = nextStart)
            tailRequests(nextRequest :: requests)
          } else requests
        }

        if (head.values.length < head.total) tailRequests(Nil).reverse else Nil
      }
      ZIO.scoped(ZIO.logSpan(s"getChangelog $key") {
        for {
          headResult <- doGetChangelogs(0, 100, key)
          _ <- ZIO.logDebug(
            s"Got ${headResult.values.length} of ${headResult.total} changelogs in head"
          )
          tailResults <- ZIO.foreachPar(tail(headResult))(req =>
            doGetChangelogs(req.startAt, req.maxResults, req.issueIdOrKey)
          )
          tailChangelogs = tailResults.flatMap(_.values)
          _ <- ZIO.logDebug(s"Got ${tailChangelogs.length} changelogs in tail")
        } yield {
          headResult.values ++ tailChangelogs
        }
      })
    }

    private def doGetChangelogs(
        startAt: Int,
        maxResults: Int,
        issueIdOrKey: String
    ): ZIO[Scope, Throwable, PageBean[Changelog]] = {
      for {
        _ <- ZIO.logDebug(
          s"Requesting changelogs ${startAt + 1} to ${startAt + maxResults}"
        )
        res <- platformClient
          .addHeader(ContentType(MediaType.application.json))
          .addPath("issue")
          .addPath(issueIdOrKey)
          .get("/changelog")
        results <- res.body.to[PageBean[Changelog]]
        _ <- ZIO.logDebug(
          s"Got changelogs ${results.startAt + 1} to ${results.startAt + results.values.length}"
        )
      } yield {
        results
      }
    }

    override def getComments(key: String): Task[List[Comment]] = {
      def tail(head: PageOfComments) = {
        @tailrec
        def tailRequests(
            requests: List[PageRequest]
        ): List[PageRequest] = {
          val lastRequest = requests match {
            case Nil       => PageRequest(head.startAt, head.maxResults, key)
            case last :: _ => last
          }

          val nextStart = lastRequest.startAt + lastRequest.maxResults

          if (nextStart < head.total) {
            val nextRequest = lastRequest.copy(startAt = nextStart)
            tailRequests(nextRequest :: requests)
          } else requests
        }

        if (head.comments.length < head.total) tailRequests(Nil).reverse
        else Nil
      }
      ZIO.scoped(ZIO.logSpan(s"getComments $key") {
        for {
          headResult <- doGetComments(key, 0, 100)
          _ <- ZIO.logDebug(
            s"Got ${headResult.comments.length} of ${headResult.total} comments in head"
          )
          tailResults <- ZIO.foreachPar(tail(headResult))(req =>
            doGetComments(req.issueIdOrKey, req.startAt, req.maxResults)
          )
          tailChangelogs = tailResults.flatMap(_.comments)
          _ <- ZIO.logDebug(s"Got ${tailChangelogs.length} comments in tail")
        } yield {
          headResult.comments ++ tailChangelogs
        }
      })
    }

    private def doGetComments(
        issueIdOrKey: String,
        startAt: Int,
        maxResults: Int
    ) = {
      for {
        _ <- ZIO.logDebug(
          s"Requesting comments ${startAt + 1} to ${startAt + maxResults}"
        )
        res <- platformClient
          .addHeader(ContentType(MediaType.application.json))
          .addPath("issue")
          .addPath(issueIdOrKey)
          .addQueryParam("expand", "renderedBody")
          .get("/comment")
        results <- res.body.to[PageOfComments]
        _ <- ZIO.logDebug(
          s"Got comments ${results.startAt + 1} to ${results.startAt + results.comments.length}"
        )
      } yield {
        results
      }
    }

    override def search(jql: String): Task[List[Issue]] = {
      val request = SearchRequest(jql, Issue.FieldNames, maxResults = 100)

      ZIO.scoped(ZIO.logSpan("search") {
        for {
          _ <- ZIO.logDebug(s"jql: $jql")
          result <- doSearch(request, 1)
          _ <- ZIO.logDebug(
            s"Got ${result.length} issues"
          )
        } yield {
          result
        }
      })
    }

    private def doSearch(
        request: SearchRequest,
        page: Int
    ): ZIO[Scope, Throwable, List[Issue]] = ZIO.logSpan(s"page $page") {
      for {
        res <- platformClient
          .addHeader(ContentType(MediaType.application.json))
          .post("/search/jql")(Body.from(request))
        results <- res.body.to[SearchResults]
        _ <- ZIO.logDebug(
          s"Got ${results.length} results in page $page."
        )
        nextPage <- results.nextPageToken match {
          case None  => ZIO.succeed(Nil)
          case token => doSearch(request.copy(nextPageToken = token), page + 1)
        }
      } yield {
        results.issues ++ nextPage
      }
    }

    override def getIssue(key: String): Task[Issue] =
      ZIO.scoped(ZIO.logSpan(s"getIssue($key)") {
        for {
          response <- platformClient.addPath("issue").get(key)
          result <- response.body.to[Issue]
        } yield result
      })

    override def addLabel(key: String, label: String): Task[Unit] =
      ZIO.scoped(ZIO.logSpan("addLabel") {
        for {
          _ <- platformClient
            .addHeader(ContentType(MediaType.application.json))
            .addPath("issue")
            .put(key)(Body.from(EditIssueRequest.addLabel(label)))
        } yield ()
      })

    override def removeLabel(key: String, label: String): Task[Unit] =
      ZIO.scoped(ZIO.logSpan("removeLabel") {
        for {
          _ <- platformClient
            .addHeader(ContentType(MediaType.application.json))
            .addPath("issue")
            .put(key)(Body.from(EditIssueRequest.removeLabel(label)))
        } yield ()
      })

    private def rankIssues(
        keys: List[String],
        after: Option[String],
        before: Option[String],
        rankCustomFieldId: Option[Int]
    ) = ZIO.scoped(
      for {
        _ <- softwareClient
          .addHeader(ContentType(MediaType.application.json))
          .addPath("issue")
          .put("rank")(
            Body.from(RankIssuesRequest(keys, after, before, rankCustomFieldId))
          )
      } yield ()
    )

    override def rankIssuesBefore(
        issues: List[String],
        reference: String,
        rankCustomFieldId: Option[Int]
    ): Task[Unit] = ZIO.logSpan("rankIssuesBefore")(
      rankIssues(issues, None, Some(reference), rankCustomFieldId)
    )

    override def rankIssuesAfter(
        issues: List[String],
        reference: String,
        rankCustomFieldId: Option[Int]
    ): Task[Unit] = ZIO.logSpan("rankIssuesAfter")(
      rankIssues(issues, Some(reference), None, rankCustomFieldId)
    )
  }

  def layer(conf: Conf) = ZLayer {
    val loggingAspect = ZClientAspect.requestLogging(
      level = {
        case _: Status.Error => LogLevel.Warning
        case _               => LogLevel.Debug
      },
      logRequestBody = true,
      logResponseBody = true
    )

    for {
      version <- ZIO
        .attempt({
          // read version info from JAR manifest
          val pak = Client.getClass.getPackage
          Option(pak.getImplementationVersion)
        })
        .orElseSucceed(None)
      headers = Headers(
        UserAgent(ProductOrComment.Product("atbp", version)),
        Authorization.Basic(conf.user, conf.token),
        Accept(MediaType.application.json)
      )
      url <- ZIO.fromEither(URL.decode(s"https://${conf.site}"))
      client <- ZIO.serviceWith[HttpClient](
        _.addHeaders(headers).url(
          url
        ) @@ loggingAspect @@ StatusCheck.successOnly()
      )
    } yield {
      LiveImpl(client): Client
    }
  }
}
