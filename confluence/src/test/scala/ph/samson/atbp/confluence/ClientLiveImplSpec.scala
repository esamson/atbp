package ph.samson.atbp.confluence

import better.files.File
import better.files.Resource
import com.atlassian.adf.jackson2.AdfJackson2
import com.atlassian.adf.model.node.*
import com.atlassian.adf.model.node.Doc.doc
import com.atlassian.adf.model.node.Media.fileMedia
import com.atlassian.adf.model.node.MediaSingle.mediaSingle
import com.atlassian.adf.model.node.Paragraph.p
import ph.samson.atbp.confluence.model.*
import ph.samson.atbp.confluence.model.UpdatePageRequest.Version
import zio.ZIO
import zio.test.*
import zio.test.TestAspect.ifPropSet

import scala.util.Random

object ClientLiveImplSpec extends ConfluenceSpec {

  val parser = new AdfJackson2()

  override def spec = suite("Confluence Client")(
    test("create page") {
      val rnd = Random.alphanumeric.take(4).mkString
      val create = for {
        client <- ZIO.service[Client]
        strings <- TestStrings
        result <- client.createPage(
          CreatePageRequest(
            spaceId = strings("spaceId"),
            status = "current",
            title = s"Test: create page $rnd",
            parentId = strings("parentId"),
            body = PageBodyWrite(
              representation = "atlas_doc_format",
              value = parser.marshall(
                doc(
                  p(s"Hello $rnd")
                )
              )
            )
          )
        )
        _ <- ZIO.log(s"result\n${pprint(result)}")
      } yield result

      for (s <- Live.live(create).exit) yield assertTrue(s.isSuccess)
    },
    test("create draft page") {
      val rnd = Random.alphanumeric.take(4).mkString
      val create = for {
        client <- ZIO.service[Client]
        strings <- TestStrings
        result <- client.createPage(
          CreatePageRequest(
            spaceId = strings("spaceId"),
            status = "draft",
            title = s"Test: create draft page $rnd",
            parentId = strings("parentId"),
            body = PageBodyWrite(
              representation = "atlas_doc_format",
              value = parser.marshall(
                doc(
                  p(s"Hello $rnd")
                )
              )
            )
          ),
          isPrivate = true
        )
        _ <- ZIO.log(s"result\n${pprint(result)}")
      } yield result

      for (s <- Live.live(create).exit) yield assertTrue(s.isSuccess)
    },
    test("create with attachment") {
      val rnd = Random.alphanumeric.take(4).mkString
      val create = for {
        client <- ZIO.service[Client]
        strings <- TestStrings
        page <- client.createPage(
          CreatePageRequest(
            spaceId = strings("spaceId"),
            status = "draft",
            title = s"Test: create with attachment $rnd",
            parentId = strings("parentId"),
            body = PageBodyWrite(
              representation = "atlas_doc_format",
              value = parser.marshall(
                doc(
                  p(s"Hello $rnd")
                )
              )
            )
          ),
          isPrivate = true
        )
        result <- client.createOrUpdateAttachment(
          page,
          Resource.url("test.png").map(u => File(u)).get
        )
        _ <- ZIO.log(s"result\n${pprint(result)}")
      } yield result

      for (s <- Live.live(create).exit) yield assertTrue(s.isSuccess)
    },
    test("publish with attachment") {
      val rnd = Random.alphanumeric.take(4).mkString
      val create = for {
        client <- ZIO.service[Client]
        strings <- TestStrings
        draft <- client.createPage(
          CreatePageRequest(
            spaceId = strings("spaceId"),
            status = "draft",
            title = s"Test: publish with attachment $rnd",
            parentId = strings("parentId"),
            body = PageBodyWrite(
              representation = "atlas_doc_format",
              value = parser.marshall(
                doc(
                  p(s"Still cooking... $rnd")
                )
              )
            )
          ),
          isPrivate = true
        )
        createAttachment <- client.createOrUpdateAttachment(
          draft,
          Resource.url("test.png").map(u => File(u)).get
        )
        attachment = createAttachment.results.head
        _ <- ZIO.log(s"attachment\n${pprint(attachment)}")
        update <- client.updatePage(
          UpdatePageRequest
            .from(draft, "test")
            .copy(
              status = "current",
              body = PageBodyWrite(
                representation = "atlas_doc_format",
                value = parser.marshall(
                  doc(
                    p(s"Cooked! $rnd"),
                    mediaSingle(fileMedia(attachment.extensions.fileId))
                  )
                )
              ),
              version = Version(1)
            )
        )
        _ <- ZIO.log(s"result\n${pprint(update)}")
        getAttachment <- client.getPageAttachments(update.id)
        _ <- ZIO.log(s"got attachment\n${pprint(getAttachment)}")
      } yield attachment

      for (s <- Live.live(create).exit) yield assertTrue(s.isSuccess)
    }
  ) @@ ifPropSet(SysProps.Site)
    @@ ifPropSet(SysProps.User)
    @@ ifPropSet(SysProps.Token)
}
