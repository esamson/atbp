package ph.samson.atbp.jira

import ph.samson.atbp.http.StatusCheck
import ph.samson.atbp.jira.model.EditIssueRequest
import ph.samson.atbp.jira.model.Issue
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
import zio.http.Response
import zio.http.Status
import zio.http.URL
import zio.http.ZClient
import zio.http.ZClientAspect

/** Jira REST API client.
  *
  * @see
  *   https://developer.atlassian.com/cloud/jira/platform/rest/
  */
trait Client {
  def getIssue(key: String): Task[Issue]
  def search(jql: String): Task[List[Issue]]
  def addLabel(key: String, label: String): Task[Unit]
  def removeLabel(key: String, label: String): Task[Unit]
}

object Client {

  private class LiveImpl(client: HttpClient) extends Client {

    override def search(jql: String): Task[List[Issue]] = {
      val request = SearchRequest(jql, Issue.FieldNames, maxResults = 100)

      def tail(head: SearchResults): List[SearchRequest] = {

        def tailRequests(
            reqs: List[SearchRequest] = Nil
        ): List[SearchRequest] = {
          val lastRequest = reqs match
            case Nil       => request
            case last :: _ => last

          val nextStart = lastRequest.startAt + lastRequest.maxResults

          if nextStart < head.total then
            val nextRequest = lastRequest.copy(startAt = nextStart)
            tailRequests(nextRequest :: reqs)
          else reqs
        }

        if head.length < head.total then tailRequests() else Nil
      }

      ZIO.scoped(ZIO.logSpan("search") {
        for {
          _ <- ZIO.logDebug(s"jql: $jql")
          headResult <- doSearch(request)
          _ <- ZIO.logDebug(
            s"Got ${headResult.issues.length} of ${headResult.total} results in head"
          )
          tailResults <- ZIO.foreachPar(tail(headResult))(doSearch)
          tailIssues = tailResults.flatMap(_.issues)
          _ <- ZIO.logDebug(s"Got ${tailIssues.length} results in tails")
        } yield {
          headResult.issues ++ tailIssues
        }
      })
    }

    private def doSearch(request: SearchRequest) = {
      for {
        _ <- ZIO.logDebug(
          s"Requesting ${request.startAt + 1} to ${request.startAt + request.maxResults}"
        )
        res <- client
          .addHeader(ContentType(MediaType.application.json))
          .post("/search")(Body.from(request))
        results <- res.body.to[SearchResults]
        _ <- ZIO.logDebug(
          s"Got results ${results.startAt + 1} to ${results.startAt + results.length}"
        )
      } yield {
        results
      }
    }

    override def getIssue(key: String): Task[Issue] =
      ZIO.scoped(ZIO.logSpan(s"getIssue($key)") {
        for {
          response <- client.addPath("issue").get(key)
          result <- response.body.to[Issue]
        } yield result
      })

    override def addLabel(key: String, label: String): Task[Unit] =
      ZIO.scoped(ZIO.logSpan("addLabel") {
        for {
          _ <- client
            .addHeader(ContentType(MediaType.application.json))
            .addPath("issue")
            .put(key)(Body.from(EditIssueRequest.addLabel(label)))
        } yield ()
      })

    override def removeLabel(key: String, label: String): Task[Unit] =
      ZIO.scoped(ZIO.logSpan("removeLabel") {
        for {
          _ <- client
            .addHeader(ContentType(MediaType.application.json))
            .addPath("issue")
            .put(key)(Body.from(EditIssueRequest.removeLabel(label)))
        } yield ()
      })
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
      url <- ZIO.fromEither(URL.decode(s"https://${conf.site}/rest/api/3"))
      client <- ZIO.serviceWith[HttpClient](
        _.addHeaders(headers).url(
          url
        ) @@ loggingAspect @@ StatusCheck.successOnly()
      )
    } yield {
      LiveImpl(client): Client
    }
    end for
  }
}
