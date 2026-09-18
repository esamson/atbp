package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.jackson2.AdfJackson2
import com.atlassian.adf.model.mark.Link
import com.atlassian.adf.model.node.Expand
import com.atlassian.adf.model.node.Text
import ph.samson.atbp.confluence.Client
import ph.samson.atbp.confluence.model.*
import zio.Ref
import zio.Task
import zio.ZIO
import zio.ZLayer
import zio.http.URL
import zio.test.*

import java.time.ZonedDateTime
import scala.jdk.CollectionConverters.*
import scala.jdk.StreamConverters.*

object PublisherLinksSpec extends ZIOSpecDefault {

  override def spec = suite("Publisher document links")(
    test("resolves forward, circular, parent and root links by source path") {
      for {
        source <- sourceTree(
          "README.md" -> "[start](Alpha.md)",
          "Alpha.md" -> "[next](nested/Target%20Page.md#details)",
          "nested/Target Page.md" ->
            """---
              |title: Custom destination
              |---
              |[back](../Alpha.md) and [home](../README.md)
              |""".stripMargin
        )
        client <- FakeClient.make
        _ <- publish(source, client)
        pages <- client.pages.get
        alpha = pages.values.find(_.title == "Alpha").get
        target = pages.values.find(_.title == "Custom destination").get
      } yield assertTrue(
        links(pages("root")) == Map("start" -> pageUrl(alpha)),
        links(alpha) == Map("next" -> s"${pageUrl(target)}#details"),
        links(target) == Map(
          "back" -> pageUrl(alpha),
          "home" -> pageUrl(pages("root"))
        )
      )
    },
    test("resolves folder notes and disambiguates identical file names") {
      for {
        source <- sourceTree(
          "README.md" -> "[left](Left/index.md) and [right](Right/Right.md)",
          "Left/index.md" -> "[local](Topic.md) and [other](../Right/Topic.md)",
          "Left/Topic.md" -> "Left topic",
          "Right/Right.md" -> "Right folder note",
          "Right/Topic.md" -> "Right topic"
        )
        client <- FakeClient.make
        _ <- publish(source, client)
        pages <- client.pages.get
        left = pages.values.find(_.title == "Left").get
        right = pages.values.find(_.title == "Right").get
        leftTopic = pages.values
          .find(p => p.title == "Topic" && p.parentId.contains(left.id))
          .get
        rightTopic = pages.values
          .find(p => p.title == "Topic" && p.parentId.contains(right.id))
          .get
      } yield assertTrue(
        links(pages("root")) == Map(
          "left" -> pageUrl(left),
          "right" -> pageUrl(right)
        ),
        links(left) == Map(
          "local" -> pageUrl(leftTopic),
          "other" -> pageUrl(rightTopic)
        )
      )
    },
    test(
      "upgrades unchanged Markdown and skips subsequent identical publishes"
    ) {
      for {
        source <- sourceTree(
          "README.md" -> "[child](Child.md)",
          "Child.md" -> "Hello"
        )
        parsed <- Parser.parse(source.root.source)
        client <- FakeClient.make
        _ <- client.pages.update(pages =>
          pages.updated(
            "root",
            pages("root").copy(version =
              pages("root").version.copy(message = parsed.contentHash)
            )
          )
        )
        _ <- publish(source, client)
        first <- client.pages.get
        _ <- publish(source, client)
        second <- client.pages.get
        child = first.values.find(_.title == "Child").get
      } yield assertTrue(
        links(first("root")) == Map("child" -> pageUrl(child)),
        first("root").version.message != parsed.contentHash,
        second == first
      )
    },
    test(
      "upgrades an unchanged Markdown page with exactly one generated footer"
    ) {
      for {
        source <- sourceTree("README.md" -> "Existing root")
        parsed <- Parser.parse(source.root.source)
        client <- FakeClient.make
        before <- client.pages.get
        _ <- client.pages.update(pages =>
          pages.updated(
            "root",
            pages("root").copy(version =
              pages("root").version.copy(message = parsed.contentHash)
            )
          )
        )
        seeded <- client.pages.get
        _ <- publish(source, client)
        first <- client.pages.get
        _ <- publish(source, client)
        second <- client.pages.get
        firstDoc = adf(first("root"))
        footer = firstDoc.content().asScala.collect { case e: Expand => e }
      } yield assertTrue(
        adf(seeded("root"))
          .content()
          .asScala
          .collect { case _: Expand =>
            ()
          }
          .isEmpty,
        first("root").version.number == before("root").version.number + 1,
        footer.size == 1,
        footer.head.title().orElse("") == Footer.Title,
        footer.head.content().asScala.map(_.toPlainText) == List(Footer.Body),
        second == first
      )
    },
    test("republishes links when a destination page is recreated") {
      for {
        source <- sourceTree(
          "README.md" -> "[child](Child.md)",
          "Child.md" -> "Hello"
        )
        client <- FakeClient.make
        _ <- publish(source, client)
        first <- client.pages.get
        child = first.values.find(_.title == "Child").get
        _ <- client.deletePage(child.id)
        _ <- publish(source, client)
        second <- client.pages.get
        replacement = second.values.find(_.title == "Child").get
      } yield assertTrue(
        replacement.id != child.id,
        links(second("root")) == Map("child" -> pageUrl(replacement)),
        second("root").version.number == first("root").version.number + 1
      )
    }
  )

  private def sourceTree(files: (String, String)*) = for {
    dir <- ZIO.acquireRelease(
      ZIO.attempt(File.newTemporaryDirectory("md2c-links"))
    )(dir => ZIO.attempt(dir.delete()).orDie)
    _ <- ZIO.attempt {
      (dir / SourceConf.FileName).writeText("spaceKey = DOC\npageId = root\n")
      files.foreach { case (path, content) =>
        val file = dir / path
        file.parent.createDirectories()
        file.writeText(content)
      }
    }
    source <- SourceTree.from(dir)
  } yield source

  private def publish(source: SourceTree, client: FakeClient) =
    ZIO
      .serviceWithZIO[Publisher](_.publish(source))
      .provide(
        ZLayer.succeed[Client](client),
        Publisher.layer(Conf.Empty)
      )

  private def links(page: PageSingle): Map[String, String] = {
    val doc = adf(page)
    doc
      .allNodesOfType(classOf[Text])
      .toScala(List)
      .flatMap { text =>
        text
          .marks(classOf[Link])
          .toScala(List)
          .map(link => text.text() -> link.href())
      }
      .toMap
  }

  private def adf(page: PageSingle) =
    new AdfJackson2().unmarshall(page.body.atlas_doc_format.get.value)

  private def pageUrl(page: PageSingle): String =
    s"https://example.atlassian.net/wiki${page._links.webui}"

  private val createdAt = ZonedDateTime.parse("2026-01-01T00:00:00Z")

  private def page(
      id: String,
      title: String,
      parent: Option[String],
      status: String,
      body: PageBodyWrite
  ): PageSingle = PageSingle(
    id,
    status,
    title,
    "space",
    parent,
    Some("page"),
    None,
    "author",
    "author",
    None,
    createdAt.toString,
    PageSingle.Version(createdAt, "", 1, false, "author"),
    PageSingle.BodySingle(
      None,
      Some(PageSingle.BodyType(body.representation, body.value)),
      None
    ),
    None,
    None,
    PageSingle.Links(s"/spaces/DOC/pages/$id", s"/pages/edit/$id", s"/x/$id")
  )

  private class FakeClient(
      val pages: Ref[Map[String, PageSingle]],
      nextId: Ref[Int]
  ) extends Client {
    override def resolveUrl(path: String): URL =
      URL
        .decode("https://example.atlassian.net/wiki")
        .toOption
        .get
        .addPath(path)

    override def getSpace(key: String): Task[Space] =
      ZIO.succeed(
        Space(
          "space",
          key,
          "Docs",
          "global",
          "author",
          createdAt,
          "root",
          "current"
        )
      )

    override def getPage(id: String): Task[PageSingle] = pages.get.map(_(id))

    override def getChildPages(id: String): Task[List[ChildPage]] =
      pages.get.map(
        _.values.filter(_.parentId.contains(id)).toList.map { page =>
          ChildPage(page.id, page.status, page.title, page.spaceId, 0)
        }
      )

    override def createPage(
        request: CreatePageRequest,
        isPrivate: Boolean
    ): Task[PageSingle] = for {
      id <- nextId.updateAndGet(_ + 1)
      created = page(
        id.toString,
        request.title,
        Some(request.parentId),
        request.status,
        request.body
      )
      _ <- pages.update(_ + (created.id -> created))
    } yield created

    override def updatePage(request: UpdatePageRequest): Task[PageSingle] =
      pages.modify { pages =>
        val current = pages(request.id)
        val updated = current.copy(
          status = request.status,
          version = current.version.copy(
            number = request.version.number,
            message = request.version.message.getOrElse("")
          ),
          body = PageSingle.BodySingle(
            None,
            Some(
              PageSingle
                .BodyType(request.body.representation, request.body.value)
            ),
            None
          )
        )
        updated -> pages.updated(updated.id, updated)
      }

    override def deletePage(id: String): Task[Boolean] =
      pages.update(_ - id).as(true)
    override def getPageAttachments(id: String): Task[List[Attachment]] =
      ZIO.succeed(Nil)
    override def getCurrentUser(): Task[User] =
      ZIO.dieMessage("Unexpected getCurrentUser")
    override def deleteDraftPage(id: String): Task[Boolean] =
      ZIO.dieMessage("Unexpected deleteDraftPage")
    override def getDraftPages(spaceKey: String): Task[List[Content]] =
      ZIO.dieMessage("Unexpected getDraftPages")
    override def createOrUpdateAttachment(
        page: PageSingle,
        file: File
    ): Task[CreateAttachmentResponse] =
      ZIO.dieMessage("Unexpected createOrUpdateAttachment")
    override def deleteAttachment(id: String): Task[Boolean] =
      ZIO.dieMessage("Unexpected deleteAttachment")
  }

  private object FakeClient {
    def make = for {
      doc <- Parser.parseMarkdown("Existing root")
      pages <- Ref.make(
        Map(
          "root" -> page(
            "root",
            "Existing root",
            None,
            "current",
            PageBodyWrite(doc)
          )
        )
      )
      nextId <- Ref.make(0)
    } yield new FakeClient(pages, nextId)
  }
}
