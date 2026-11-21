package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.jackson2.AdfJackson2
import com.atlassian.adf.model.mark.Link
import com.atlassian.adf.model.node.Extension
import com.atlassian.adf.model.node.Paragraph
import com.atlassian.adf.model.node.Rule
import com.atlassian.adf.model.node.Text
import ph.samson.atbp.confluence.Client
import ph.samson.atbp.confluence.model.*
import zio.Ref
import zio.Task
import zio.Unsafe
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
    },
    test("attaches images referenced with percent-encoded paths") {
      for {
        source <- sourceTree(
          "Note.md" -> "![encoded](Pitaka%20Ledger.png)\n"
        )
        dir = source.root.source
        _ <- ZIO.attempt {
          (dir / "Pitaka Ledger.png").writeText("encoded")
        }
        attached <- Ref.make(List.empty[File])
        client <- FakeClient.make(attached)
        _ <- publish(source, client)
        files <- attached.get
      } yield assertTrue(
        files.map(_.name) == List("Pitaka Ledger.png")
      )
    },
    test(
      "publishes through degraded metadata, skips repeats, and updates once on recovery"
    ) {
      for {
        source <- sourceTree("Note.md" -> "Hello")
        dir = source.root.source
        runner = new GitFakes.ScriptedRunner(
          dir,
          Map(
            "remote" -> new GitMetadata.TimedOut,
            "ls-files" -> new GitMetadata.TimedOut,
            "log" -> new GitMetadata.TimedOut
          )
        )
        client <- FakeClient.make()
        publishWithGit = GitMetadata.withRunner(runner)(publish(source, client))
        _ <- publishWithGit
        degraded <- client.pages.get
        note = degraded.values.find(_.title == "Note").get
        _ <- publishWithGit
        repeated <- client.pages.get
        _ = runner.fail(Map.empty)
        _ <- publishWithGit
        recovered <- client.pages.get
        _ <- publishWithGit
        stable <- client.pages.get
        degradedText = body(degraded(note.id))
        recoveredText = body(recovered(note.id))
      } yield assertTrue(
        degraded.size == 2,
        degraded.values.forall(_.status == "current"),
        degradedText.contains("generated from markdown source"),
        degradedText.contains(
          "Clone command unavailable: remote lookup failed."
        ),
        degradedText.contains("Source change status unavailable."),
        degradedText.contains("Source history unavailable."),
        body(degraded("root")).contains("generated directory listing"),
        repeated == degraded,
        recovered(note.id).version.number == degraded(
          note.id
        ).version.number + 1,
        recovered("root").version.number == degraded("root").version.number + 1,
        recoveredText.contains(GitFakes.RemoteUrl),
        recoveredText.contains("Clarify setup"),
        !recoveredText.contains("unavailable"),
        stable == recovered
      )
    },
    test("metadata failures stay sanitized while other failures propagate") {
      val secret = "https://user:s3cret-token@example.com/team/docs.git"
      for {
        source <- sourceTree("Note.md" -> "Hello")
        runner = new GitFakes.ScriptedRunner(
          source.root.source,
          Map(
            "remote" -> new RuntimeException(s"fatal: $secret"),
            "log" -> new java.io.IOException(secret)
          )
        )
        ok <- FakeClient.make()
        _ <- GitMetadata.withRunner(runner)(publish(source, ok))
        pages <- ok.pages.get
        logs <- ZTestLogger.logOutput
        rendered = logs.map(entry => s"${entry.message()} ${entry.cause}")
        broken <- FakeClient.make(
          _ => ZIO.fail(new RuntimeException("confluence boom")),
          retainBodies = true
        )
        exit <- GitMetadata.withRunner(runner)(publish(source, broken)).exit
      } yield assertTrue(
        logs.nonEmpty,
        rendered.forall(!_.contains("s3cret")),
        pages.values.forall(page => !body(page).contains("s3cret")),
        pages.values.forall(!_.version.message.contains("s3cret")),
        exit.causeOption.exists(_.prettyPrint.contains("confluence boom"))
      )
    },
    footerAwarePublication
  ) @@ GitFixtures.isolated

  private def footerAwarePublication = suite("footer-aware publication")(
    test(
      "broken Git discovery skips repeat writes and updates once after recovery"
    ) {
      val diagnostic = "fixture-only-diagnostic"
      for {
        repo <- repoWith("Note.md" -> "Hello")
        config = repo / ".git" / "config"
        brokenConfig = repo / ".git" / diagnostic
        original <- ZIO.attemptBlocking(config.contentAsString)
        _ <- ZIO.attemptBlocking {
          brokenConfig.write("[broken\n")
          config.write(s"$original\n[include]\n\tpath = $diagnostic\n")
        }
        source <- docsTree(repo)
        client <- FakeClient.make()
        snapshot = publish(source, client) *> client.pages.get
        degraded <- snapshot
        _ <- ZIO.attemptBlocking(brokenConfig.write("\n\n[broken\n"))
        repeated <- snapshot
        _ <- ZIO.attemptBlocking(config.write(original))
        recovered <- snapshot
        stable <- snapshot
        logs <- ZTestLogger.logOutput
      } yield assertTrue(
        degraded.size == 2,
        degraded.values.forall(page =>
          body(page).contains(
            "Git metadata unavailable: repository lookup failed."
          ) && !body(page).contains(diagnostic) && !page.version.message
            .contains(diagnostic)
        ),
        logs.forall(entry =>
          !s"${entry.message()} ${entry.cause}".contains(diagnostic)
        ),
        repeated == degraded,
        recovered.values.forall(page =>
          page.version.number == degraded(page.id).version.number + 1
        ),
        body(titled(recovered, "Note")).contains("docs/Note.md"),
        stable == recovered
      )
    },
    test("a legacy page gains its footer once, then skips") {
      for {
        repo <- repoWith("Note.md" -> "Hello")
        source <- docsTree(repo)
        client <- FakeClient.make()
        _ <- publish(source, client)
        first <- client.pages.get
        note = titled(first, "Note")
        parsed <- Parser.parse(repo / "docs" / "Note.md")
        legacyBody <- Parser.parseMarkdown("Hello")
        // pre-feature state: hash of the Markdown alone, no footer in the body
        _ <- client.pages.update(
          _.updated(
            note.id,
            note.copy(
              version = note.version.copy(message = parsed.contentHash),
              body = PageSingle.BodySingle(
                None,
                Some(
                  PageSingle.BodyType(
                    "atlas_doc_format",
                    PageBodyWrite(legacyBody).value
                  )
                ),
                None
              )
            )
          )
        )
        legacy <- client.pages.get
        _ <- publish(source, client)
        upgraded <- client.pages.get
        _ <- publish(source, client)
        repeated <- client.pages.get
      } yield assertTrue(
        !body(legacy(note.id)).contains("Do not edit here"),
        upgraded(note.id).version.number == note.version.number + 1,
        upgraded(note.id).version.message != parsed.contentHash,
        oneFooter(upgraded(note.id)),
        repeated == upgraded
      )
    },
    test("footer facts and source history decide whether a page updates") {
      for {
        repo <- repoWith("Note.md" -> "Hello")
        docs = repo / "docs"
        client <- FakeClient.make()
        // re-evaluated on every run: the publication root moves in the last step
        snapshot = ZIO.suspend {
          for {
            source <- SourceTree.from(
              if (docs.exists) docs else repo / "handbook"
            )
            _ <- publish(source, client)
            pages <- client.pages.get
          } yield pages
        }
        v0 <- snapshot
        noteId = titled(v0, "Note").id
        rootVersion = (p: Map[String, PageSingle]) => p("root").version.number
        noteVersion = (p: Map[String, PageSingle]) => p(noteId).version.number
        noteText = (p: Map[String, PageSingle]) => body(p(noteId))

        // identical run
        v1 <- snapshot
        // unrelated commit: no page text changes
        _ <- ZIO.attemptBlocking {
          GitFixtures.commitFile(
            repo,
            "other.txt",
            "changed\n",
            "Unrelated",
            "2026-02-01T10:00:00Z"
          ): Unit
        }
        v2 <- snapshot
        // a commit that only adds front matter leaves the body hash alone
        _ <- ZIO.attemptBlocking {
          GitFixtures.commitFile(
            repo,
            "docs/Note.md",
            "---\ntitle: Note\n---\nHello",
            "Add front matter",
            "2026-02-02T10:00:00Z"
          ): Unit
        }
        v3 <- snapshot
        v4 <- snapshot
        // worktree mode change: only the uncommitted status differs
        _ <- ZIO.attemptBlocking {
          (repo / "docs" / "Note.md").toJava.setExecutable(true): Unit
        }
        v5 <- snapshot
        v6 <- snapshot
        _ <- ZIO.attemptBlocking {
          (repo / "docs" / "Note.md").toJava.setExecutable(false): Unit
        }
        v7 <- snapshot
        // clone command change reaches every page showing the command
        _ <- ZIO.attemptBlocking {
          GitFixtures.run(
            repo,
            "remote",
            "set-url",
            "origin",
            "git@example.com:team/moved.git"
          )
        }
        v8 <- snapshot
        // remote availability change
        _ <- ZIO.attemptBlocking {
          GitFixtures.run(repo, "remote", "remove", "origin")
        }
        v9 <- snapshot
        v10 <- snapshot
        // path and history change together when the directory is renamed
        _ <- ZIO.attemptBlocking {
          GitFixtures.run(repo, "mv", "docs", "handbook")
          GitFixtures.commitAll(repo, "Rename docs", "2026-02-03T10:00:00Z")
        }
        v11 <- snapshot
        v12 <- snapshot
      } yield assertTrue(
        v1 == v0,
        v2 == v0,
        noteVersion(v3) == noteVersion(v2) + 1,
        noteText(v3).contains("Add front matter"),
        rootVersion(v3) == rootVersion(v2),
        v4 == v3,
        noteVersion(v5) == noteVersion(v4) + 1,
        noteText(v5).contains("Source has uncommitted changes"),
        rootVersion(v5) == rootVersion(v4),
        v6 == v5,
        noteVersion(v7) == noteVersion(v6) + 1,
        !noteText(v7).contains("uncommitted"),
        noteVersion(v8) == noteVersion(v7) + 1,
        rootVersion(v8) == rootVersion(v7) + 1,
        noteText(v8).contains("moved.git"),
        noteVersion(v9) == noteVersion(v8) + 1,
        rootVersion(v9) == rootVersion(v8) + 1,
        noteText(v9).contains("no Git remote configured"),
        v10 == v9,
        noteVersion(v11) == noteVersion(v10) + 1,
        rootVersion(v11) == rootVersion(v10) + 1,
        noteText(v11).contains("handbook/Note.md"),
        noteText(v11).contains("Rename docs"),
        noteText(v11).contains("Add front matter"),
        v12 == v11,
        v11.values.forall(oneFooter)
      )
    },
    test("recreated link destinations still invalidate footer-bearing pages") {
      for {
        repo <- repoWith(
          "README.md" -> "[child](Child.md)",
          "Child.md" -> "Hello"
        )
        source <- docsTree(repo)
        client <- FakeClient.make()
        _ <- publish(source, client)
        first <- client.pages.get
        child = titled(first, "Child")
        _ <- client.deletePage(child.id)
        _ <- publish(source, client)
        second <- client.pages.get
        replacement = titled(second, "Child")
        _ <- publish(source, client)
        third <- client.pages.get
      } yield assertTrue(
        replacement.id != child.id,
        links(second("root")) == Map("child" -> pageUrl(replacement)),
        second("root").version.number == first("root").version.number + 1,
        second.values.forall(oneFooter),
        third == second
      )
    },
    test("changed attachments update an otherwise unchanged footer page") {
      for {
        repo <- repoWith(
          "Pic.md" -> "![pic](pic.png)\n",
          "pic.png" -> "first image"
        )
        source <- docsTree(repo)
        client <- FakeClient.make()
        _ <- publish(source, client)
        first <- client.pages.get
        pic = titled(first, "Pic")
        _ <- publish(source, client)
        second <- client.pages.get
        stored1 <- client.storedAttachments.get
        // the image is not the Markdown source: no commit and no status change
        _ <- ZIO.attemptBlocking((repo / "docs" / "pic.png").write("second"))
        _ <- publish(source, client)
        third <- client.pages.get
        stored2 <- client.storedAttachments.get
        _ <- publish(source, client)
        fourth <- client.pages.get
      } yield assertTrue(
        stored1.map(_.title) == List("pic.png"),
        second == first,
        third(pic.id).version.number == first(pic.id).version.number + 1,
        third(pic.id).version.message == first(pic.id).version.message,
        third("root") == first("root"),
        stored2.map(_.comment) != stored1.map(_.comment),
        stored2.size == 1,
        oneFooter(third(pic.id)),
        fourth == third
      )
    },
    test(
      "keeps listing, trimming, exclusions and children-before-parent order"
    ) {
      for {
        repo <- repoWith(
          "Guide/Intro.md" -> "# Intro\n\nBody text",
          "Guide/Notes.md" -> "Notes",
          "Guide/data.txt" -> "not a page"
        )
        _ <- ZIO.attemptBlocking(
          (repo / "docs" / "Empty").createDirectories(): Unit
        )
        source <- docsTree(repo)
        order <- Ref.make(List.empty[String])
        client <- FakeClient.make(
          onUpdate = request =>
            order.update(_ :+ s"${request.title}:${request.body.value.length}"),
          retainBodies = true
        )
        _ <- publish(source, client)
        pages <- client.pages.get
        updates <- order.get
        titles = pages.values.map(_.title).toSet
        guide = titled(pages, "Guide")
        intro = titled(pages, "Intro")
        introDoc = new AdfJackson2().unmarshall(body(intro))
        guideDoc = new AdfJackson2().unmarshall(body(guide))
        finalIndex = (title: String) =>
          updates.lastIndexWhere(_.startsWith(s"$title:"))
      } yield assertTrue(
        titles == Set("Existing root", "Guide", "Intro", "Notes"),
        introDoc.content().get(0).getClass == classOf[Paragraph],
        body(intro).contains("Body text"),
        !body(intro).contains("\"heading\""),
        guideDoc.content().get(0).getClass == classOf[Extension],
        guideDoc.content().get(1).getClass == classOf[Rule],
        pages.values.forall(oneFooter),
        finalIndex("Intro") < finalIndex("Guide"),
        finalIndex("Notes") < finalIndex("Guide"),
        finalIndex("Guide") < finalIndex("Existing root")
      )
    },
    test("completed bodies are not retained and repeats skip") {
      for {
        repo <- repoWith("A.md" -> "Alpha", "B/C.md" -> "Gamma")
        source <- docsTree(repo)
        client <- FakeClient.make(_ => ZIO.unit, retainBodies = false)
        _ <- publish(source, client)
        first <- client.pages.get
        _ <- publish(source, client)
        second <- client.pages.get
      } yield assertTrue(
        first.size == 4,
        first.values.forall(_.body.atlas_doc_format.isEmpty),
        first.values.forall(_.version.message.nonEmpty),
        second == first
      )
    }
  )

  private def body(page: PageSingle): String =
    page.body.atlas_doc_format.map(_.value).getOrElse("")

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

  private def titled(pages: Map[String, PageSingle], title: String) =
    pages.values.find(_.title == title).get

  /** Exactly one footer: a single separator and a single page-type warning. */
  private def oneFooter(page: PageSingle): Boolean = {
    val text = body(page)
    val doc = new AdfJackson2().unmarshall(text)
    doc.allNodesOfType(classOf[Rule]).count() == 1 &&
    "Do not edit here\\.".r.findAllIn(text).size == 1
  }

  /** A committed repository whose publication root is `docs/`. */
  private def repoWith(files: (String, String)*) = for {
    repo <- GitFixtures.temp
    _ <- ZIO.attemptBlocking {
      GitFixtures.init(repo)
      GitFixtures.run(repo, "remote", "add", "origin", GitFakes.RemoteUrl)
      val all = Seq(
        s"docs/${SourceConf.FileName}" -> "spaceKey = DOC\npageId = root\n",
        "other.txt" -> "other\n"
      ) ++ files.map { case (path, content) => s"docs/$path" -> content }
      all.foreach { case (path, content) =>
        val file = repo / path
        file.parent.createDirectories()
        file.write(content)
      }
      GitFixtures.commitAll(repo, "Initial", "2026-01-01T10:00:00Z")
    }
  } yield repo

  private def docsTree(repo: File) = SourceTree.from(repo / "docs")

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
      retainBodies: Boolean,
      attached: Ref[List[File]]
  ) extends Client {
    val storedAttachments: Ref[List[Attachment]] =
      Unsafe.unsafe(implicit u => Ref.unsafe.make(List.empty[Attachment]))

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
          ChildPage(page.id, page.status, page.title, page.spaceId, Some(0))
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
      storedAttachments.get.map(_.filter(_.pageId == id))
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
      attached.update(file :: _) *>
        storedAttachments
          .update(
            _.filterNot(a => a.pageId == page.id && a.title == file.name) :+
              storedAttachment(page.id, file)
          )
          .as(attachmentResponse(file))
    override def deleteAttachment(id: String): Task[Boolean] =
      storedAttachments.update(_.filterNot(_.id == id)).as(true)
  }

  private object FakeClient {
    def make(): Task[FakeClient] = make(_ => ZIO.unit, retainBodies = true)

    def make(attached: Ref[List[File]]): Task[FakeClient] =
      make(_ => ZIO.unit, retainBodies = true, attached)

    def make(
        onUpdate: UpdatePageRequest => Task[Unit],
        retainBodies: Boolean
    ): Task[FakeClient] =
      Ref.make(List.empty[File]).flatMap(make(onUpdate, retainBodies, _))

    def make(
        onUpdate: UpdatePageRequest => Task[Unit],
        retainBodies: Boolean,
        attached: Ref[List[File]]
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
      retainBodies,
      attached
    )
  }

  private def storedAttachment(pageId: String, file: File): Attachment =
    Attachment(
      mediaTypeDescription = None,
      webuiLink = s"/attachments/${file.name}",
      createdAt = createdAt,
      comment = file.sha1,
      version = Attachment.Version(1, "", false, "author", createdAt),
      title = file.name,
      status = "current",
      fileSize = 1,
      fileId = s"file-${file.name}",
      mediaType = "image/png",
      pageId = pageId,
      downloadLink = s"/download/${file.name}",
      id = s"att-${file.name}",
      _links = Attachment.Links(s"/download/${file.name}", s"/a/${file.name}")
    )

  private def attachmentResponse(file: File): CreateAttachmentResponse = {
    val user = CreateAttachmentResponse.User(
      `type` = "known",
      accountId = "author",
      accountType = "atlassian",
      email = "author@example.com",
      publicName = "Author",
      timeZone = "UTC",
      displayName = "Author",
      isExternalCollaborator = false,
      isGuest = false
    )
    CreateAttachmentResponse(
      results = List(
        CreateAttachmentResponse.Content(
          id = s"att-${file.name}",
          `type` = "attachment",
          status = "current",
          title = file.name,
          version = CreateAttachmentResponse.Version(
            by = user,
            when = createdAt,
            friendlyWhen = "just now",
            message = "",
            number = 1,
            minorEdit = false,
            contentTypeModified = false
          ),
          container = CreateAttachmentResponse.Container(
            id = "page",
            `type` = "page",
            status = "current",
            title = "Page"
          ),
          metadata = CreateAttachmentResponse.Metadata(
            comment = "",
            mediaType = "image/png"
          ),
          extensions = CreateAttachmentResponse.Extensions(
            mediaType = "image/png",
            fileSize = 1,
            comment = "",
            mediaTypeDescription = None,
            fileId = s"file-${file.name}",
            collectionName = "contentId-page"
          )
        )
      ),
      start = None,
      limit = None,
      size = 1
    )
  }
}
