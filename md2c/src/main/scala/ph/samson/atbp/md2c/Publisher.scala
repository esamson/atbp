package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Media
import com.atlassian.adf.model.node.Media.ExternalMedia
import com.atlassian.adf.model.node.Media.fileMedia
import ph.samson.atbp.confluence.Client
import ph.samson.atbp.confluence.model.Attachment
import ph.samson.atbp.confluence.model.ChildPage
import ph.samson.atbp.confluence.model.CreatePageRequest
import ph.samson.atbp.confluence.model.PageBodyWrite
import ph.samson.atbp.confluence.model.PageSingle
import ph.samson.atbp.confluence.model.Space
import ph.samson.atbp.confluence.model.UpdatePageRequest
import ph.samson.atbp.md2c.SourceTree.Node
import ph.samson.atbp.md2c.StagedTree.Page
import zio.Ref
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

    private case class PreparedPage(
        node: Node,
        current: PageSingle,
        currentChildren: List[ChildPage],
        children: List[PreparedPage]
    ) {
      def allPages: List[PreparedPage] = this :: children.flatMap(_.allPages)
    }

    // Bound the renderers and attachment operations within each page as well.
    override def publish(source: SourceTree): Task[PageSingle] =
      ZIO
        .logSpan("publish") {
          for {
            finalConf <- conf.finalConf(source.conf)
            given Space <- client.getSpace(finalConf.spaceKey)
            rootPage <- client.getPage(finalConf.pageId).map(withoutBody)
            baseDir =
              if (source.root.source.isDirectory) source.root.source
              else source.root.source.parent
            sourcePages <- collectSourcePages(source.root, baseDir)
            assignedTitles = PageTitles.assign(sourcePages)
            _ <- logSuffixedTitles(sourcePages, assignedTitles)
            // Reserve every ID before resolving links, including forward references.
            // A title that already exists elsewhere in the space is reused so a
            // later sibling does not keep the title while a new draft is published.
            reserved <- Ref.make(Set(rootPage.id))
            prepared <- preparePage(
              source.root,
              rootPage,
              assignedTitles,
              reserved
            )
            pageUrls = prepared.allPages.map { page =>
              page.node.source -> client
                .resolveUrl(page.current._links.webui)
                .encode
            }.toMap
            latest <- Ref.make(
              prepared.allPages
                .map(page => page.current.id -> page.current)
                .toMap
            )
            reservedIds <- reserved.get
            published <- publishTree(
              prepared,
              None,
              pageUrls,
              baseDir,
              latest,
              reservedIds
            )
          } yield published
        }
        .withParallelism(1)

    // Responses can include large bodies. Only metadata is needed after each
    // request; retaining the bodies here would grow heap with the whole tree.
    private def withoutBody(page: PageSingle): PageSingle =
      page.copy(body = PageSingle.BodySingle(None, None, None))

    private def preparePage(
        node: Node,
        current: PageSingle,
        assignedTitles: Map[File, String],
        reserved: Ref[Set[String]]
    )(using Space): Task[PreparedPage] =
      for {
        currentChildren <- client.getChildPages(current.id)
        children <- ZIO.foreach(Page.children(node)) { child =>
          for {
            confluenceTitle <- assignedConfluenceTitle(child, assignedTitles)
            page <- currentChildren.find(_.title == confluenceTitle) match {
              case Some(existing) =>
                reserved.update(_ + existing.id) *>
                  client.getPage(existing.id).map(withoutBody)
              case None =>
                adoptOrCreate(confluenceTitle, current.id, reserved)
            }
            prepared <- preparePage(child, page, assignedTitles, reserved)
          } yield prepared
        }
      } yield PreparedPage(node, current, currentChildren, children)

    /** Direct-child match missed. Reuse a page in the space with this exact
      * title, or create a draft under `parentId`.
      */
    private def adoptOrCreate(
        title: String,
        parentId: String,
        reserved: Ref[Set[String]]
    )(using space: Space): Task[PageSingle] =
      for {
        taken <- reserved.get
        existing <- findReusable(title, taken)
        page <- existing match {
          case Some(page) =>
            ZIO.logInfo(
              s"""reusing Confluence page ${page.id} "$title""""
            ) *> reserved.update(_ + page.id).as(page)
          case None =>
            createDraft(title, parentId)
              .map(withoutBody)
              .tap(page => reserved.update(_ + page.id))
        }
      } yield page

    private def findReusable(title: String, taken: Set[String])(using
        space: Space
    ): Task[Option[PageSingle]] =
      for {
        current <- client.findPagesByTitle(space.key, title, "current")
        found <- current.find(page =>
          page.title == title && !taken.contains(page.id)
        ) match {
          case Some(page) => ZIO.succeed(Some(page))
          case None       =>
            client
              .findPagesByTitle(space.key, title, "draft")
              .map(
                _.find(page => page.title == title && !taken.contains(page.id))
              )
        }
        page <- ZIO.foreach(found)(page =>
          client.getPage(page.id).map(withoutBody)
        )
      } yield page

    private def relativeSourcePath(baseDir: File, source: File): String =
      if (baseDir == source) baseDir.name
      else
        baseDir.path.relativize(source.path).toString.replace('\\', '/')

    private def collectSourcePages(
        node: Node,
        baseDir: File
    ): Task[List[PageTitles.SourcePage]] =
      ZIO
        .foreach(Page.children(node)) { child =>
          for {
            frontMatter <- child match {
              case _: Node.Directory => ZIO.succeed(Parser.FrontMatter.Empty)
              case _                 => Parser.readFrontMatter(child.source)
            }
            logicalTitle = frontMatter.title.getOrElse(child.name)
            page = PageTitles.SourcePage(
              child.source,
              relativeSourcePath(baseDir, child.source),
              logicalTitle
            )
            rest <- collectSourcePages(child, baseDir)
          } yield page :: rest
        }
        .map(_.flatten)

    private def logSuffixedTitles(
        sourcePages: List[PageTitles.SourcePage],
        assignedTitles: Map[File, String]
    ): Task[Unit] =
      ZIO.foreachDiscard(sourcePages) { page =>
        val confluenceTitle = assignedTitles(page.source)
        val suffixCount = confluenceTitle.length - page.logicalTitle.length
        if (suffixCount > 0)
          ZIO.logInfo(
            s"""duplicate Confluence title: ${page.relativePath} "${page.logicalTitle}" + $suffixCount"""
          )
        else ZIO.unit
      }

    private def assignedConfluenceTitle(
        child: Node,
        assignedTitles: Map[File, String]
    ): Task[String] =
      ZIO
        .fromOption(assignedTitles.get(child.source))
        .orElseFail(
          new IllegalStateException(
            s"No assigned Confluence title for ${child.source.pathAsString}"
          )
        )

    private def publishTree(
        prepared: PreparedPage,
        parentId: Option[String],
        pageUrls: Map[File, String],
        baseDir: File,
        latest: Ref[Map[String, PageSingle]],
        reservedIds: Set[String]
    ): Task[PageSingle] = for {
      // Finish children before rendering their parent, and discard responses
      // immediately. Neither wide nor deep trees retain completed page bodies.
      _ <- ZIO.foreachDiscard(prepared.children)(
        publishTree(
          _,
          Some(prepared.current.id),
          pageUrls,
          baseDir,
          latest,
          reservedIds
        )
      )
      page <- Page.convertPage(prepared.node, pageUrls)
      published <- publishPage(
        page,
        prepared,
        baseDir,
        parentId,
        latest,
        reservedIds
      )
    } yield published

    private def publishPage(
        page: Page,
        prepared: PreparedPage,
        baseDir: File,
        parentId: Option[String],
        latest: Ref[Map[String, PageSingle]],
        reservedIds: Set[String]
    ): Task[PageSingle] = {
      val sourceName =
        if (baseDir == page.source) baseDir.name
        else baseDir.relativize(page.source)
      val publish = for {
        _ <- ensureParentCurrent(prepared.current.id, parentId, latest)
        current <- latest.get.map(_(prepared.current.id))
        currentAttachments <- client.getPageAttachments(current.id)
        attachmentsAreUpToDate = {
          val hashes = currentAttachments.map(_.comment)
          mediaFiles(page.adf, page.source).forall { case (_, file) =>
            hashes.contains(file.sha1)
          }
        }

        parentChanged = parentId.exists(current.parentId != Some(_))
        needsUpdate =
          current.version.message != page.contentHash || !attachmentsAreUpToDate
        published <- {
          if (current.isDraft || needsUpdate || parentChanged)
            updatePage(
              current,
              page.trimmedTitle,
              currentAttachments,
              parentId
            ).tap(updated =>
              latest.update(_.updated(updated.id, withoutBody(updated)))
            )
          else
            ZIO.logInfo(
              s"up to date: $sourceName -> ${client.resolveUrl(current._links.tinyui)}"
            ) *> ZIO.succeed(current)
        }

        _ <-
          if (needsUpdate) {
            if (current.isDraft) {
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

        extraChildren =
          prepared.currentChildren.map(_.id).filterNot(reservedIds.contains)
        _ <- deletePages(extraChildren)
      } yield published

      publish.mapError(cause => PublishingFailed(page, cause))
    }

    def createDraft(title: String, parentId: String)(using
        space: Space
    ) = {
      for {
        result <- client.createPage(
          CreatePageRequest(
            spaceId = space.id,
            status = "draft",
            title = title,
            parentId = parentId,
            body = PageBodyWrite(Doc.doc())
          )
        )
      } yield result
    }

    def mediaFiles(doc: Doc, source: File): Map[String, File] = {
      val base = if (source.isDirectory) source else source.parent

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
        if (path.isAbsolute) {
          url -> File(path)
        } else {
          url -> File(base.path.resolve(path))
        }
      }

      externalFiles.toMap
    }

    /** A current page cannot be moved under a draft. Publish the new parent
      * first, and keep its version so the later body update does not go stale.
      */
    private def ensureParentCurrent(
        pageId: String,
        parentId: Option[String],
        latest: Ref[Map[String, PageSingle]]
    ): Task[Unit] =
      for {
        current <- latest.get.map(_(pageId))
        moving = parentId.exists(current.parentId != Some(_))
        _ <- ZIO.foreachDiscard(parentId.filter(_ => moving)) { pid =>
          for {
            parent <- latest.get.map(_(pid))
            _ <- ZIO.when(parent.isDraft) {
              client
                .updatePage(
                  UpdatePageRequest
                    .from(parent, parent.version.message)
                    .copy(body = PageBodyWrite(Doc.doc()))
                )
                .map(withoutBody)
                .tap(published =>
                  latest.update(_.updated(published.id, published))
                )
            }
          } yield ()
        }
      } yield ()

    def updatePage(
        current: PageSingle,
        update: Page,
        currentAttachments: List[Attachment],
        parentId: Option[String]
    ): Task[PageSingle] = for {
      // map from external url -> (file ID, attachment ID)
      externalFileId: Map[String, (String, String)] <- {
        val z = for {
          (url, file) <- mediaFiles(update.adf, update.source)
        } yield {
          currentAttachments.find(_.title == file.name) match {
            case Some(attachment) =>
              if (attachment.comment == file.sha1)
                ZIO.succeed(url -> (attachment.fileId, attachment.id))
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
          .copy(
            body = PageBodyWrite(attached),
            parentId = parentId.orElse(current.parentId)
          )
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
