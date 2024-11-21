package ph.samson.atbp.confluence

import better.files.File
import ph.samson.atbp.confluence.model.Attachment
import ph.samson.atbp.confluence.model.ChildPagesResult
import ph.samson.atbp.confluence.model.ChildPagesResult.ChildPage
import ph.samson.atbp.confluence.model.Content
import ph.samson.atbp.confluence.model.CreateAttachmentResponse
import ph.samson.atbp.confluence.model.CreatePageRequest
import ph.samson.atbp.confluence.model.MultiEntityLinks
import ph.samson.atbp.confluence.model.MultiEntityResult
import ph.samson.atbp.confluence.model.PageSingle
import ph.samson.atbp.confluence.model.Space
import ph.samson.atbp.confluence.model.UpdatePageRequest
import ph.samson.atbp.confluence.model.User
import ph.samson.atbp.http.StatusCheck
import zio.LogLevel
import zio.Task
import zio.ZIO
import zio.ZLayer
import zio.http.Body
import zio.http.Client as HttpClient
import zio.http.Form
import zio.http.FormField
import zio.http.FormField.Simple
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
import zio.http.ZClientAspect
import zio.schema.codec.BinaryCodec
import zio.stream.ZStream

/** Confluence REST API client.
  *
  * @see
  *   https://developer.atlassian.com/cloud/confluence/rest/v2/
  */
trait Client {

  def resolveUrl(path: String): URL

  def getCurrentUser(): Task[User]

  def getSpace(key: String): Task[Space]

  def createPage(
      request: CreatePageRequest,
      isPrivate: Boolean = false
  ): Task[PageSingle]

  def getPage(id: String): Task[PageSingle]

  def updatePage(request: UpdatePageRequest): Task[PageSingle]

  def deletePage(id: String): Task[Boolean]

  def deleteDraftPage(id: String): Task[Boolean]

  def getChildPages(id: String): Task[List[ChildPage]]

  def getDraftPages(spaceKey: String): Task[List[Content]]

  def createOrUpdateAttachment(
      page: PageSingle,
      file: File
  ): Task[CreateAttachmentResponse]

  def deleteAttachment(id: String): Task[Boolean]

  def getPageAttachments(id: String): Task[List[Attachment]]
}

object Client {

  private def V1Base = "/rest/api"
  private def V2Base = "/api/v2"

  private class LiveImpl(client: HttpClient, baseUrl: URL) extends Client {

    override def resolveUrl(path: String): URL = baseUrl.addPath(path)

    override def getCurrentUser(): Task[User] =
      ZIO.scoped(ZIO.logSpan("getCurrentUser") {
        for {
          res <- client
            .addPath(V1Base)
            .addPath("user")
            .get("current")
          result <- res.body.to[User]
        } yield result
      })

    override def getSpace(key: String): Task[Space] =
      ZIO.scoped(ZIO.logSpan("getSpace") {
        for {
          _ <- ZIO.logDebug(s"getSpace $key")
          res <- client
            .addPath(V2Base)
            .addQueryParam("keys", key)
            .get("/spaces")
          results <- res.body.to[MultiEntityResult[Space]]
          space <- ZIO
            .fromOption(results.results.headOption)
            .orElseFail(new NoSuchElementException(s"No space with key $key"))
        } yield space
      })

    override def createPage(
        request: CreatePageRequest,
        isPrivate: Boolean
    ): Task[PageSingle] =
      ZIO.scoped(ZIO.logSpan("createPage") {
        for {
          _ <- ZIO.logDebug(s"createPage $request")
          res <- client
            .addPath(V2Base)
            .addQueryParam("private", isPrivate.toString)
            .addHeader(ContentType(MediaType.application.json))
            .post("/pages")(Body.from(request))
          result <- res.body.to[PageSingle]
        } yield result
      })

    override def getPage(id: String): Task[PageSingle] =
      ZIO.scoped(ZIO.logSpan("getPage") {
        for {
          _ <- ZIO.logDebug(s"getPage $id")
          res <- client
            .addPath(V2Base)
            .addPath("/pages")
            .get(id)
          result <- res.body.to[PageSingle]
        } yield result
      })

    override def updatePage(request: UpdatePageRequest): Task[PageSingle] =
      ZIO.scoped(ZIO.logSpan("updatePage") {
        for {
          _ <- ZIO.logDebug(s"updatePage $request")
          res <- client
            .addPath(V2Base)
            .addHeader(ContentType(MediaType.application.json))
            .addPath("/pages")
            .put(request.id)(Body.from(request))
          result <- res.body.to[PageSingle]
        } yield result
      })

    override def deletePage(id: String): Task[Boolean] =
      ZIO.scoped(ZIO.logSpan("deletePage") {
        for {
          _ <- ZIO.logDebug(s"deletePage $id")
          res <- client
            .addPath(V2Base)
            .addPath("/pages")
            .delete(id)
          result = res.status == Status.NoContent
        } yield result
      })

    override def deleteDraftPage(id: String): Task[Boolean] =
      ZIO.scoped(ZIO.logSpan("deleteDraftPage") {
        for {
          _ <- ZIO.logDebug(s"deleteDraftPage $id")
          res <- client
            .addPath(V2Base)
            .addPath("/pages")
            .addQueryParam("draft", "true")
            .delete(id)
          result = res.status == Status.NoContent
        } yield result
      })

    override def getChildPages(id: String): Task[List[ChildPage]] =
      ZIO.scoped(ZIO.logSpan("getChildPages") {
        for {
          _ <- ZIO.logDebug(s"getChildPages $id")
          res <- client
            .addPath(V2Base)
            .addPath("/pages")
            .addPath(id)
            .get("children")
          result <- res.body.to[ChildPagesResult]
          _ <- ZIO.logDebug(s"result: $result")
          next <- getNextChildPages(result._links)
        } yield result.results ++ next
      })

    override def getDraftPages(spaceKey: String): Task[List[Content]] =
      ZIO.scoped(ZIO.logSpan("getDraftPages") {
        for {
          _ <- ZIO.logDebug(s"getDraftPages $spaceKey")
          res <- client
            .addPath(V1Base)
            .addQueryParam("type", "page")
            .addQueryParam("spaceKey", spaceKey)
            .addQueryParam("status", "draft")
            .get("content")
          result <- res.body.to[MultiEntityResult[Content]]
          _ <- ZIO.logDebug(s"result: $result")
          next <- getNextPage[Content](result._links)
        } yield result.results ++ next
      })

    private def getNextChildPages(
        links: MultiEntityLinks
    ): Task[List[ChildPage]] =
      links.next match
        case None => ZIO.succeed(Nil)
        case Some(next) =>
          ZIO.scoped(ZIO.logSpan("getNextChildPages") {
            for {
              _ <- ZIO.logDebug("getNextChildPages")
              res <- client
                .get(next)
              result <- res.body.to[ChildPagesResult]
              _ <- ZIO.logDebug(s"result: $result")
              next <- getNextChildPages(result._links)
            } yield result.results ++ next
          })

    override def getPageAttachments(id: String): Task[List[Attachment]] =
      ZIO.scoped(ZIO.logSpan("getPageAttachments") {
        for {
          _ <- ZIO.logDebug(s"getPageAttachments $id")
          res <- client
            .addPath(V2Base)
            .addPath("/pages")
            .addPath(id)
            .get("attachments")
          result <- res.body.to[MultiEntityResult[Attachment]]
          _ <- ZIO.logDebug(s"result: $result")
          next <- getNextPage[Attachment](result._links)
        } yield result.results ++ next
      })

    private def getNextPage[T](links: MultiEntityLinks)(implicit
        codec: BinaryCodec[MultiEntityResult[T]]
    ): Task[List[T]] =
      links.next match
        case None => ZIO.succeed(Nil)
        case Some(next) =>
          ZIO.scoped(ZIO.logSpan("getNextPage") {
            for {
              _ <- ZIO.logDebug("getNextPage")
              res <- client
                .get(next)
              result <- res.body.to[MultiEntityResult[T]]
              _ <- ZIO.logDebug(s"result: $result")
              next <- getNextPage(result._links)
            } yield result.results ++ next
          })

    override def createOrUpdateAttachment(
        page: PageSingle,
        file: File
    ): Task[CreateAttachmentResponse] =
      ZIO.scoped(ZIO.logSpan("createOrUpdateAttachment") {
        val data = ZStream.fromPath(file.path).orElseSucceed(Byte.MinValue)
        val mediaType = file
          .extension(
            includeDot = false,
            includeAll = true,
            toLowerCase = true
          )
          .flatMap(MediaType.forFileExtension)
          .getOrElse(MediaType.application.`octet-stream`)
        for {
          body <- Body.fromMultipartFormUUID(
            Form(
              FormField.StreamingBinary(
                name = "file",
                contentType = mediaType,
                filename = Some(file.name),
                data = data
              ),
              Simple("comment", file.sha1),
              Simple("minorEdit", "true")
            )
          )
          res <- client
            .addHeader("X-Atlassian-Token", "nocheck")
            .addPath(V1Base)
            .addPath("content")
            .addPath(page.id)
            .addQueryParam("status", page.status)
            .put("/child/attachment")(body)
          result <- res.body.to[CreateAttachmentResponse]
        } yield result
        end for
      })

    override def deleteAttachment(id: String): Task[Boolean] =
      ZIO.scoped(ZIO.logSpan("deleteAttachment") {
        for {
          _ <- ZIO.logDebug(s"deleteAttachment $id")
          res <- client
            .addPath(V2Base)
            .addPath("/attachments")
            .delete(id)
          result = res.status == Status.NoContent
        } yield result
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
      url <- ZIO.fromEither(URL.decode(s"https://${conf.site}/wiki"))
      client <- ZIO.serviceWith[HttpClient](
        _.addHeaders(headers).url(url) @@ loggingAspect @@ StatusCheck
          .successOnly()
      )
    } yield LiveImpl(client, url): Client
    end for
  }
}
