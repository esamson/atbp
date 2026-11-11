package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.jackson2.AdfJackson2
import com.atlassian.adf.model.mark.Link
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
        client <- FakeClient.make()
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
        client <- FakeClient.make()
        _ <- publish(source, client)
        pages <- client.pages.get
        left = pages.values.find(_.title == "Left").get
        right = pages.values.find(_.title == "Right").get
        leftTopic = pages.values
          .find(p => p.title == "Topic" && p.parentId.contains(left.id))
          .get
        zws = PageTitles.ZeroWidthSpace
        rightTopic = pages.values
          .find(p => p.title == s"Topic$zws" && p.parentId.contains(right.id))
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
    test("republishes duplicate titles without changing page ids") {
      val zws = PageTitles.ZeroWidthSpace
      for {
        source <- sourceTree(
          "Left/index.md" -> "Left folder",
          "Left/Topic.md" -> "Left topic",
          "Right/Right.md" -> "Right folder note",
          "Right/Topic.md" -> "Right topic"
        )
        client <- FakeClient.make()
        _ <- publish(source, client)
        first <- client.pages.get
        _ <- publish(source, client)
        second <- client.pages.get
        left = first.values.find(_.title == "Left").get
        right = first.values.find(_.title == "Right").get
        leftTopic = first.values
          .find(p => p.title == "Topic" && p.parentId.contains(left.id))
          .get
        rightTopic = first.values
          .find(p => p.title == s"Topic$zws" && p.parentId.contains(right.id))
          .get
      } yield assertTrue(
        first("root").title == "Existing root",
        second == first,
        second(rightTopic.id).title == s"Topic$zws",
        second(leftTopic.id).title == "Topic"
      )
    },
    test("reuses a title that already exists under a later sibling") {
      val zws = PageTitles.ZeroWidthSpace
      for {
        source <- sourceTree(
          "Core Accounting and Transactions/Onboarding/Ledger Onboarding.md" ->
            "Ledger body",
          "Fincore Back-Office Tools/Onboarding/Account Assets Onboarding.md" ->
            "Assets body"
        )
        client <- FakeClient.make()
        empty <- Parser.parseMarkdown("")
        _ <- client.pages.update { pages =>
          val fincore = page(
            "fincore",
            "Fincore Back-Office Tools",
            Some("root"),
            "current",
            PageBodyWrite(empty)
          )
          val onboarding = page(
            "onboarding",
            "Onboarding",
            Some("fincore"),
            "current",
            PageBodyWrite(empty)
          )
          val ledger = page(
            "ledger",
            "Ledger Onboarding",
            Some("onboarding"),
            "current",
            PageBodyWrite(empty)
          )
          pages ++ Map(
            fincore.id -> fincore,
            onboarding.id -> onboarding,
            ledger.id -> ledger
          )
        }
        _ <- publish(source, client)
        pages <- client.pages.get
        core = pages.values
          .find(_.title == "Core Accounting and Transactions")
          .get
        onboarding = pages("onboarding")
        ledger = pages("ledger")
        suffixed = pages.values.find(_.title == s"Onboarding$zws").get
      } yield assertTrue(
        pages.values.map(_.title).count(_ == "Ledger Onboarding") == 1,
        pages.values.map(_.title).count(_ == "Onboarding") == 1,
        onboarding.parentId.contains(core.id),
        ledger.parentId.contains(onboarding.id),
        suffixed.parentId.contains("fincore")
      )
    },
    test("suffixes folder notes after lexicographically earlier siblings") {
      val zws = PageTitles.ZeroWidthSpace
      for {
        source <- sourceTree(
          "README.md" -> "Root body",
          "Topic.md" -> "Bare topic",
          "nested/README.md" ->
            """---
              |title: Topic
              |---
              |Nested folder note
              |""".stripMargin
        )
        client <- FakeClient.make()
        _ <- publish(source, client)
        pages <- client.pages.get
        bare = pages.values.find(_.title == "Topic").get
        suffixed = pages.values.find(_.title == s"Topic$zws").get
      } yield assertTrue(
        bare.parentId.contains(pages("root").id),
        suffixed.parentId.contains(pages("root").id),
        bare.id != suffixed.id
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
        client <- FakeClient.make()
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
    test("republishes links when a destination page is recreated") {
      for {
        source <- sourceTree(
          "README.md" -> "[child](Child.md)",
          "Child.md" -> "Hello"
        )
        client <- FakeClient.make()
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
    },
    test("removes obsolete children after publishing the remaining tree") {
      for {
        source <- sourceTree(
          "Keep.md" -> "Keep this page",
          "Remove/Old.md" -> "Remove this subtree"
        )
        client <- FakeClient.make()
        _ <- publish(source, client)
        _ <- ZIO.attempt((source.root.source / "Remove").delete())
        remaining <- SourceTree.from(source.root.source)
        _ <- publish(remaining, client)
        pages <- client.pages.get
      } yield assertTrue(
        pages.size == 2,
        pages.values.map(_.title).toSet == Set("Existing root", "Keep")
      )
    },
    test("reserves IDs with empty draft bodies") {
      for {
        source <- sourceTree("Child.md" -> "A large body " * 1000)
        client <- FakeClient.make()
        _ <- publish(source, client)
        sizes <- client.draftBodySizes.get
      } yield assertTrue(sizes.size == 1, sizes.forall(_ < 100))
    },
    test("converts siblings and ancestors only when ready to publish them") {
      for {
        source <- sourceTree(
          "README.md" -> "Original root",
          "A.md" -> "First child",
          "B.md" -> "Original sibling"
        )
        client <- FakeClient.make(
          onUpdate = request =>
            ZIO.attempt {
              if (request.title == "A") {
                source.root.source.writeText("Updated root")
                (source.root.source.parent / "B.md")
                  .writeText("Updated sibling"): Unit
              }
            },
          retainBodies = true
        )
        _ <- publish(source, client)
        pages <- client.pages.get
        sibling = pages.values.find(_.title == "B").get
      } yield assertTrue(
        pages("root").body.atlas_doc_format.get.value.contains("Updated root"),
        sibling.body.atlas_doc_format.get.value.contains("Updated sibling")
      )
    },
    test("publishes a large document set") {
      largeDocumentSet(8).map(result => assertTrue(result))
    }
  )

  private[md2c] def largeDocumentSet(count: Int) = {
    val markdown = "```text\n" + ("large document body\n" * 14000) + "```\n"
    for {
      source <- sourceTree((1 to count).map(i => s"Page-$i.md" -> markdown)*)
      // The remote server owns published bodies, not the test process.
      client <- FakeClient.make(_ => ZIO.unit, retainBodies = false)
      _ <- publish(source, client)
      pages <- client.pages.get
    } yield {
      pages.size == count + 1 &&
      pages.values.forall(_.status == "current") &&
      pages.values.forall(_.version.message.nonEmpty)
    }
  }

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
    val doc = new AdfJackson2().unmarshall(page.body.atlas_doc_format.get.value)
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
      nextId: Ref[Int],
      val draftBodySizes: Ref[List[Int]],
      onUpdate: UpdatePageRequest => Task[Unit],
      retainBodies: Boolean
  ) extends Client {
    private def stored(page: PageSingle): PageSingle =
      if (retainBodies) page
      else page.copy(body = PageSingle.BodySingle(None, None, None))

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
      _ <- draftBodySizes.update(request.body.value.length :: _)
      _ <- pages.update(_ + (created.id -> stored(created)))
    } yield created

    override def updatePage(request: UpdatePageRequest): Task[PageSingle] =
      pages.get.flatMap { existing =>
        val current = existing(request.id)
        val expectedVersion =
          if (current.status == "draft") 1 else current.version.number + 1
        val moving = request.parentId.exists(current.parentId != Some(_))
        val targetIsDraft = request.parentId
          .flatMap(existing.get)
          .exists(_.status == "draft")
        val titleTaken = request.status == "current" && existing.values.exists {
          page =>
            page.id != request.id && page.status == "current" &&
            page.title == request.title
        }
        if (request.version.number != expectedVersion)
          ZIO.fail(
            new IllegalStateException(
              s"version ${request.version.number} for ${request.id}, expected $expectedVersion"
            )
          )
        else if (moving && targetIsDraft)
          ZIO.fail(
            new IllegalStateException(
              s"cannot move ${request.id} under draft ${request.parentId}"
            )
          )
        else if (titleTaken)
          ZIO.fail(
            new IllegalStateException(
              s"A page with this title already exists: ${request.title}"
            )
          )
        else
          onUpdate(request) *> pages.modify { pages =>
            val current = pages(request.id)
            val updated = current.copy(
              status = request.status,
              title = request.title,
              parentId = request.parentId.orElse(current.parentId),
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
            updated -> pages.updated(updated.id, stored(updated))
          }
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

    override def findPagesByTitle(
        spaceKey: String,
        title: String,
        status: String
    ): Task[List[Content]] =
      pages.get.map { existing =>
        existing.values.toList.collect {
          case page if page.title == title && page.status == status =>
            Content(page.id, "page", page.status, page.title)
        }
      }
    override def createOrUpdateAttachment(
        page: PageSingle,
        file: File
    ): Task[CreateAttachmentResponse] =
      ZIO.dieMessage("Unexpected createOrUpdateAttachment")
    override def deleteAttachment(id: String): Task[Boolean] =
      ZIO.dieMessage("Unexpected deleteAttachment")
  }

  private object FakeClient {
    def make(): Task[FakeClient] = make(_ => ZIO.unit, retainBodies = true)

    def make(
        onUpdate: UpdatePageRequest => Task[Unit],
        retainBodies: Boolean
    ) = for {
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
      draftBodySizes <- Ref.make(List.empty[Int])
    } yield new FakeClient(
      pages,
      nextId,
      draftBodySizes,
      onUpdate,
      retainBodies
    )
  }
}
