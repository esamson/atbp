package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Media
import com.atlassian.adf.model.node.Media.ExternalMedia
import com.atlassian.adf.model.node.Media.fileMedia
import ph.samson.atbp.confluence.Client
import ph.samson.atbp.confluence.model.Attachment
import ph.samson.atbp.confluence.model.ChildPagesResult.ChildPage
import ph.samson.atbp.confluence.model.CreatePageRequest
import ph.samson.atbp.confluence.model.PageBodyWrite
import ph.samson.atbp.confluence.model.PageSingle
import ph.samson.atbp.confluence.model.Space
import ph.samson.atbp.confluence.model.UpdatePageRequest
import ph.samson.atbp.md2c.StagedTree.Page
import zio.Task
import zio.ZIO
import zio.ZLayer

import java.nio.file.Paths
import scala.jdk.FunctionConverters.*
import scala.jdk.StreamConverters.*

trait Publisher {
  def publish(source: SourceTree): Task[PageSingle]
}

object Publisher {

  private class LiveImpl(client: Client, conf: Conf) extends Publisher {

    override def publish(source: SourceTree): Task[PageSingle] =
      ZIO.scoped(ZIO.logSpan("publish") {
        for {
          staged <- StagedTree.from(source)
          finalConf <- conf.finalConf(source.conf)
          given Conf.Final = finalConf
          given Space <- client.getSpace(finalConf.spaceKey)
          rootPage <- client.getPage(finalConf.pageId)
          rootParent = rootPage.parentId
          rootChildren <- rootParent
            .map(client.getChildPages)
            .getOrElse(ZIO.succeed(Nil))
          baseDir =
            if staged.root.source.isDirectory
            then staged.root.source
            else staged.root.source.parent
          published <- rootParent match
            case Some(parentId) =>
              publishPage(staged.root, parentId, rootChildren, baseDir)
            case None =>
              publishPage(staged.root, rootPage, baseDir)
        } yield published
      })

    def publishPage(
        page: Page,
        parentId: String,
        currentPages: List[ChildPage],
        baseDir: File
    )(using
        conf: Conf.Final,
        space: Space
    ): Task[PageSingle] = {

      for {
        current <- currentPage(page, parentId, currentPages)
        published <- publishPage(page, current, baseDir)
      } yield published
    }

    def publishPage(
        page: Page,
        current: PageSingle,
        baseDir: File
    )(using
        conf: Conf.Final,
        space: Space
    ): Task[PageSingle] = {
      val sourceName =
        if baseDir == page.source then baseDir.name
        else baseDir.relativize(page.source)
      val publish = for {
        currentChildren <- client.getChildPages(current.id)
        publishedChildren <- ZIO.foreachPar(page.children)(
          publishPage(_, current.id, currentChildren, baseDir)
        )
        currentAttachments <- client.getPageAttachments(current.id)
        attachmentsAreUpToDate = {
          val hashes = currentAttachments.map(_.comment)
          mediaFiles(page.adf, page.source).forall { case (_, file) =>
            hashes.contains(file.sha1)
          }
        }

        needsUpdate =
          current.version.message != page.contentHash || !attachmentsAreUpToDate
        published <- {
          if current.isDraft || needsUpdate
          then updatePage(current, page.trimmedTitle, currentAttachments)
          else
            ZIO.logInfo(
              s"up to date: $sourceName -> ${client.resolveUrl(current._links.tinyui)}"
            ) *> ZIO.succeed(current)
        }

        _ <-
          if needsUpdate then {
            if current.isDraft then {
              ZIO.logInfo(
                s"published: $sourceName -> ${client.resolveUrl(published._links.tinyui)}"
              )
            } else {
              ZIO.logInfo(
                s"updated: $sourceName -> ${client.resolveUrl(published._links.tinyui)}"
              )
            }
          } else {
            ZIO.unit
          }

        extraChildren = {
          val publishedIds = published.id :: publishedChildren.map(_.id)
          currentChildren.map(_.id).filterNot(publishedIds.contains)
        }
        _ <- deletePages(extraChildren)
      } yield published

      publish.mapError(cause => PublishingFailed(page, cause))
    }

    def currentPage(
        page: Page,
        parentId: String,
        currentPages: List[ChildPage]
    )(using conf: Conf.Final, space: Space) = {
      currentPages.find(_.title == page.title).map(_.id) match
        case Some(id) => getPage(id)
        case None     => createDraft(page, parentId)
    }

    def getPage(id: String)(using conf: Conf.Final) = {
      for {
        result <- client.getPage(id)
      } yield result
    }

    def createDraft(page: Page, parentId: String)(using space: Space) = {
      for {
        result <- client.createPage(
          CreatePageRequest(
            spaceId = space.id,
            status = "draft",
            title = page.title,
            parentId = parentId,
            body = PageBodyWrite(page.adf)
          )
        )
      } yield result
    }

    def mediaFiles(doc: Doc, source: File): Map[String, File] = {
      val base = if source.isDirectory then source else source.parent

      // the urls in the media nodes point to the files to be attached
      val externalUrls = doc
        .allNodesOfType(classOf[ExternalMedia])
        .map(_.url())
        .toScala(List)

      // relative urls are resolved against the page source file to come up with
      // the actual file to attach
      val externalFiles = for {
        url <- externalUrls
      } yield {
        val path = Paths.get(url)
        if path.isAbsolute then {
          url -> File(path)
        } else {
          url -> File(base.path.resolve(path))
        }
      }

      externalFiles.toMap
    }

    def updatePage(
        current: PageSingle,
        update: Page,
        currentAttachments: List[Attachment]
    ): Task[PageSingle] = for {
      // map from external url -> (file ID, attachment ID)
      externalFileId: Map[String, (String, String)] <- {
        val z = for {
          (url, file) <- mediaFiles(update.adf, update.source)
        } yield {
          currentAttachments.find(_.title == file.name) match
            case Some(attachment) =>
              if attachment.comment == file.sha1
              then ZIO.succeed(url -> (attachment.fileId, attachment.id))
              else
                for {
                  attach <- client.createOrUpdateAttachment(current, file)
                } yield {
                  val attachment = attach.results.head
                  url -> (attachment.extensions.fileId, attachment.id)
                }
            case None =>
              for {
                attach <- client.createOrUpdateAttachment(current, file)
              } yield {
                val attachment = attach.results.head
                url -> (attachment.extensions.fileId, attachment.id)
              }
        }
        ZIO.collectAllPar(z).map(_.toMap)
      }

      attached = {
        val transformer: Media => Media = {
          case external: ExternalMedia =>
            fileMedia(externalFileId(external.url())._1)
          case other => other
        }
        update.adf.transformDescendants(
          classOf[Media],
          transformer.asJavaFunction
        )
        update.adf
      }
      update <- client.updatePage(
        UpdatePageRequest
          .from(current, update.contentHash)
          .copy(body = PageBodyWrite(attached))
      )

      extraAttachments = {
        val latest = externalFileId.values.map(_._2).toList
        val extras = currentAttachments.filterNot { attachment =>
          latest.contains(attachment.id)
        } map (_.id)
        extras
      }
      _ <- deleteAttachments(extraAttachments)
    } yield update

    def deletePages(ids: List[String]): Task[Boolean] = for {
      dels <- ZIO.foreachPar(ids) { id =>
        for {
          children <- client.getChildPages(id)
          childDels <- deletePages(children.map(_.id))
          del <- client.deletePage(id)
        } yield childDels && del
      }
    } yield dels.forall(_ == true)

    def deleteAttachments(ids: List[String]): Task[Boolean] = for {
      dels <- ZIO.foreachPar(ids)(client.deleteAttachment)
    } yield dels.forall(_ == true)
  }

  def layer(conf: Conf) = ZLayer {
    for {
      client <- ZIO.service[Client]
    } yield LiveImpl(client, conf): Publisher
  }

  case class PublishingFailed(page: Page, cause: Throwable)
      extends Exception(
        s"Failed publishing ${page.title} (${page.source})",
        cause
      )
}
