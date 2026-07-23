package ph.samson.atbp.liga.serve

import better.files.File
import ph.samson.atbp.liga.io.PeriodWriter
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.serve.ApiJson.*
import ph.samson.atbp.liga.serve.Routes as LigaRoutes
import ph.samson.atbp.liga.testsupport.RaceToTestSupport
import ph.samson.atbp.liga.tournament.EventCodec
import ph.samson.atbp.liga.tournament.EventLog
import ph.samson.atbp.liga.tournament.PeriodEmission
import ph.samson.atbp.liga.tournament.Replay
import ph.samson.atbp.liga.tournament.Resume
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.*
import zio.http.*
import zio.json.*
import zio.test.*

import java.net.InetAddress
import java.time.Instant
import java.time.LocalDate

object WriteApiSpec extends ZIOSpecDefault {

  private val at = Instant.parse("2026-03-15T18:00:00Z")

  private val loopback: Option[InetAddress] =
    Some(InetAddress.getLoopbackAddress)

  private val remote: Option[InetAddress] =
    Some(InetAddress.getByName("192.168.1.50"))

  private def localhostPost(path: String, body: String): Request =
    Request
      .post(path, Body.fromString(body))
      .copy(remoteAddress = loopback)

  private def localhostGet(path: String): Request =
    Request.get(path).copy(remoteAddress = loopback)

  private def remotePost(path: String, body: String): Request =
    Request
      .post(path, Body.fromString(body))
      .copy(remoteAddress = remote)

  private def withTempTournament(
      seeded: Boolean
  ): ZIO[Any, Throwable, (ServeContext, File)] =
    ZIO.attempt {
      val root = File.newTemporaryDirectory("liga-write-api")
      val dataDir = root / "data"
      val tournamentDir = root / "tournament-test"
      dataDir.createDirectoryIfNotExists()
      tournamentDir.createDirectoryIfNotExists()
      File(getClass.getResource("/periods/eight-player-seed.liga"))
        .copyTo(dataDir / "eight-player-seed.liga")
      val players = (1 to 8).map(i => Player(s"P$i")).toList
      val wizardEvents = List(
        TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload(
            name = "Spring Open",
            players = Nil
          )
        ),
        TournamentEvent.PlayersSet(
          seq = 2,
          at = at,
          payload = PlayersSetPayload(players = players)
        ),
        TournamentEvent.PlayersLocked(
          seq = 3,
          at = at,
          payload = PlayersLockedPayload()
        )
      )
      if (seeded) {
        File(getClass.getResource("/tournaments/eight-player-seeded"))
          .copyTo(tournamentDir, overwrite = true)
      } else {
        wizardEvents.foreach { event =>
          (tournamentDir / EventLog.filenameFor(event))
            .write(EventCodec.encode(event))
        }
      }
      val ctx =
        ServeContext(dataDir = dataDir, tournamentDir = Some(tournamentDir))
      (ctx, root)
    }

  private def cleanup(root: File): UIO[Unit] =
    ZIO.attemptBlocking(root.delete(swallowIOExceptions = true)).unit.orDie

  private val eightRaceToByScopeBody =
    """{"raceToByScope":{"wb-1":7,"wb-2":7,"wb-3":7,"lb-1":7,"lb-2":7,"lb-3":7,"lb-4":7,"gf":7}}"""

  private def configureRaceTo(
      ctx: ServeContext
  ): ZIO[Scope, Throwable, Response] =
    LigaRoutes
      .routes(ctx, BindConfig())
      .runZIO(localhostPost("/api/tournament/race-to", eightRaceToByScopeBody))

  private def seedTournament(
      ctx: ServeContext
  ): ZIO[Scope, Throwable, Response] =
    for {
      _ <- configureRaceTo(ctx)
      response <- LigaRoutes
        .routes(ctx, BindConfig())
        .runZIO(localhostPost("/api/tournament/seed", "{}"))
    } yield response

  private def playMatch(
      ctx: ServeContext,
      matchId: String
  ): ZIO[Scope, Throwable, TournamentResponse] =
    for {
      ready <- LigaRoutes
        .routes(ctx, BindConfig())
        .runZIO(localhostPost(s"/api/matches/$matchId/ready", ""))
      readyBody <- ready.body.asString
      afterReady <- ZIO.fromEither(
        readyBody
          .fromJson[TournamentResponse]
          .left
          .map(msg => new RuntimeException(msg))
      )
      suggested = afterReady.bracket
        .flatMap(_.matches.find(_.id == matchId))
        .flatMap(_.handicapSuggested)
        .getOrElse(0)
      _ <- LigaRoutes
        .routes(ctx, BindConfig())
        .runZIO(
          localhostPost(
            s"/api/matches/$matchId/handicap",
            s"""{"handicap":$suggested}"""
          )
        )
      _ <- LigaRoutes
        .routes(ctx, BindConfig())
        .runZIO(localhostPost(s"/api/matches/$matchId/start", ""))
      result <- LigaRoutes
        .routes(ctx, BindConfig())
        .runZIO(
          localhostPost(
            s"/api/matches/$matchId/result",
            """{"scoreA":7,"scoreB":4}"""
          )
        )
      resultBody <- result.body.asString
      finalState <- ZIO.fromEither(
        resultBody
          .fromJson[TournamentResponse]
          .left
          .map(msg => new RuntimeException(msg))
      )
    } yield finalState

  private def playAllReadyMatches(
      ctx: ServeContext,
      state: TournamentResponse
  ): ZIO[Scope, Throwable, TournamentResponse] = {
    val readyIds =
      state.bracket.toList
        .flatMap(_.matches)
        .filter(_.state == BracketMatchState.Ready)
        .map(_.id)
    if (readyIds.isEmpty) {
      ZIO.succeed(state)
    } else {
      for {
        afterRound <- ZIO.foldLeft(readyIds)(state) { (_, matchId) =>
          playMatch(ctx, matchId)
        }
        finalState <- playAllReadyMatches(ctx, afterRound)
      } yield finalState
    }
  }

  def spec = suite("WriteApi")(
    test("POST /api/tournament/create rejects incomplete tournament with 400") {
      for {
        root <- ZIO.attemptBlocking(
          File.newTemporaryDirectory("liga-create-incomplete")
        )
        dataDir = root / "data"
        _ <- ZIO.attemptBlocking {
          dataDir.createDirectoryIfNotExists()
          File(getClass.getResource("/periods/eight-player-seed.liga"))
            .copyTo(dataDir / "eight-player-seed.liga")
          val tournamentDir =
            dataDir / Resume.tournamentDirName("Spring Open", LocalDate.now())
          tournamentDir.createDirectoryIfNotExists()
          val created = TournamentEvent.Created(
            seq = 1,
            at = at,
            payload = TournamentCreatedPayload(
              name = "Spring Open",
              players = Nil
            )
          )
          (tournamentDir / EventLog.filenameFor(created))
            .write(EventCodec.encode(created))
        }
        ctx = ServeContext(dataDir = dataDir, tournamentDir = None)
        response <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/tournament/create",
              """{"name":"Another Open"}"""
            )
          )
        body <- response.body.asString
        _ <- cleanup(root)
      } yield assertTrue(
        response.status == Status.BadRequest,
        body.contains(
          "an incomplete tournament already exists; resume or remove it first"
        )
      )
    },
    test(
      "POST /api/tournament/create returns 409 when tournament dir already exists"
    ) {
      val createdOn = LocalDate.now()
      for {
        root <- ZIO.attemptBlocking(
          File.newTemporaryDirectory("liga-create-collision")
        )
        dataDir = root / "data"
        _ <- ZIO.attemptBlocking {
          dataDir.createDirectoryIfNotExists()
          val dirName = Resume.tournamentDirName("Spring Open", createdOn)
          File(getClass.getResource("/tournaments/eight-player-complete"))
            .copyTo(dataDir / dirName, overwrite = true)
        }
        ctx = ServeContext(dataDir = dataDir, tournamentDir = None)
        response <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/tournament/create",
              """{"name":"Spring Open"}"""
            )
          )
        body <- response.body.asString
        _ <- cleanup(root)
      } yield assertTrue(
        response.status == Status.Conflict,
        body.contains("pick a different tournament name")
      )
    },
    test("wizard create → players → lock → race-to → seed flow") {
      for {
        root <- ZIO.attemptBlocking(
          File.newTemporaryDirectory("liga-wizard-api")
        )
        dataDir = root / "data"
        _ <- ZIO.attemptBlocking {
          dataDir.createDirectoryIfNotExists()
          File(getClass.getResource("/periods/eight-player-seed.liga"))
            .copyTo(dataDir / "eight-player-seed.liga")
        }
        ctx = ServeContext(dataDir = dataDir, tournamentDir = None)
        playersJson = (1 to 8)
          .map(i => s"""{"name":"P$i"}""")
          .mkString("[", ",", "]")
        create <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/tournament/create",
              """{"name":"Spring Open"}"""
            )
          )
        createdBody <- create.body.asString
        created <- ZIO.fromEither(createdBody.fromJson[TournamentResponse])
        _ <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/tournament/players",
              s"""{"players":$playersJson}"""
            )
          )
        _ <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/tournament/lock", "{}"))
        _ <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/tournament/race-to",
              """{"raceToByScope":{"wb-1":7,"wb-2":7,"wb-3":7,"lb-1":7,"lb-2":7,"lb-3":7,"lb-4":7,"gf":7}}"""
            )
          )
        seed <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/tournament/seed", "{}"))
        seedBody <- seed.body.asString
        seeded <- ZIO.fromEither(seedBody.fromJson[TournamentResponse])
        _ <- cleanup(root)
      } yield assertTrue(
        create.status == Status.Ok,
        created.phase == "defining",
        seed.status == Status.Ok,
        seeded.phase == "active",
        seeded.bracket.exists(_.size == 8)
      )
    },
    test("3-player seed leaves wb-1-2 ready via HTTP API") {
      val threePlayerRaceTo =
        """{"raceToByScope":{"wb-1":7,"wb-2":7,"lb-1":7,"lb-2":7,"gf":7}}"""
      for {
        root <- ZIO.attemptBlocking(
          File.newTemporaryDirectory("liga-three-player-api")
        )
        dataDir = root / "data"
        _ <- ZIO.attemptBlocking {
          dataDir.createDirectoryIfNotExists()
          File(getClass.getResource("/periods/eight-player-seed.liga"))
            .copyTo(dataDir / "eight-player-seed.liga")
        }
        ctx = ServeContext(dataDir = dataDir, tournamentDir = None)
        _ <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/tournament/create",
              """{"name":"Three Player Open"}"""
            )
          )
        _ <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/tournament/players",
              """{"players":[{"name":"P1"},{"name":"P2"},{"name":"P3"}]}"""
            )
          )
        _ <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/tournament/lock", "{}"))
        _ <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/tournament/race-to", threePlayerRaceTo))
        seed <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/tournament/seed", "{}"))
        seedBody <- seed.body.asString
        seeded <- ZIO.fromEither(seedBody.fromJson[TournamentResponse])
        wb12 = seeded.bracket.flatMap(_.matches.find(_.id == "wb-1-2"))
        readyIds =
          seeded.bracket.toList.flatMap(
            _.matches.filter(_.state == BracketMatchState.Ready).map(_.id)
          )
        _ <- cleanup(root)
      } yield assertTrue(
        seed.status == Status.Ok,
        seeded.phase == "active",
        seeded.bracket.exists(_.size == 4),
        wb12.exists(_.state == BracketMatchState.Ready),
        readyIds == List("wb-1-2")
      )
    },
    test(
      "POST /api/tournament/seed creates bracket with frozen period ratings"
    ) {
      for {
        (ctx, root) <- withTempTournament(seeded = false)
        response <- seedTournament(ctx)
        body <- response.body.asString
        parsed <- ZIO.fromEither(body.fromJson[TournamentResponse])
        seededFileExists = (ctx.tournamentDir.get / "000012-seeded.json").exists
        _ <- cleanup(root)
      } yield assertTrue(
        response.status == Status.Ok,
        parsed.bracket.exists(_.size == 8),
        parsed.frozenRatings.size == 8,
        parsed.frozenRatings.map(_.player.name).sorted == (1 to 8)
          .map(i => s"P$i")
          .toList,
        parsed.raceToByScope == RaceToTestSupport.uniformRaceTo(8),
        seededFileExists
      )
    },
    test("seed → ready → handicap → start → result flow") {
      for {
        (ctx, root) <- withTempTournament(seeded = false)
        seed <- seedTournament(ctx)
        ready <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/matches/wb-1-1/ready", ""))
        readyBody <- ready.body.asString
        afterReady <- ZIO.fromEither(readyBody.fromJson[TournamentResponse])
        handicap <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/matches/wb-1-1/handicap",
              """{"handicap":3}"""
            )
          )
        start <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/matches/wb-1-1/start", ""))
        result <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/matches/wb-1-1/result",
              """{"scoreA":7,"scoreB":4}"""
            )
          )
        resultBody <- result.body.asString
        finalState <- ZIO.fromEither(resultBody.fromJson[TournamentResponse])
        matchDef = finalState.bracket
          .flatMap(_.matches.find(_.id == "wb-1-1"))
          .get
        _ <- cleanup(root)
      } yield assertTrue(
        seed.status == Status.Ok,
        ready.status == Status.Ok,
        afterReady.bracket
          .flatMap(_.matches.find(_.id == "wb-1-1"))
          .flatMap(_.handicapSuggested)
          .nonEmpty,
        handicap.status == Status.Ok,
        start.status == Status.Ok,
        result.status == Status.Ok,
        matchDef.state == BracketMatchState.Completed,
        matchDef.handicapApplied.contains(3),
        matchDef.result.contains(MatchResult(7, 4))
      )
    },
    test("write routes reject non-localhost requests with 403") {
      for {
        (ctx, root) <- withTempTournament(seeded = false)
        _ <- configureRaceTo(ctx)
        response <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(remotePost("/api/tournament/seed", "{}"))
        _ <- cleanup(root)
      } yield assertTrue(response.status == Status.Forbidden)
    },
    test("POST /api/tournament/players rejects duplicate names with 400") {
      for {
        root <- ZIO.attemptBlocking(
          File.newTemporaryDirectory("liga-dup-players")
        )
        dataDir = root / "data"
        tournamentDir = root / "tournament-test"
        _ <- ZIO.attemptBlocking {
          dataDir.createDirectoryIfNotExists()
          tournamentDir.createDirectoryIfNotExists()
          File(getClass.getResource("/periods/eight-player-seed.liga"))
            .copyTo(dataDir / "eight-player-seed.liga")
          val created = TournamentEvent.Created(
            seq = 1,
            at = at,
            payload = TournamentCreatedPayload(
              name = "Spring Open",
              players = Nil
            )
          )
          (tournamentDir / EventLog.filenameFor(created))
            .write(EventCodec.encode(created))
        }
        ctx = ServeContext(
          dataDir = dataDir,
          tournamentDir = Some(tournamentDir)
        )
        response <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/tournament/players",
              """{"players":[{"name":"Alice"},{"name":"Alice"}]}"""
            )
          )
        body <- response.body.asString
        _ <- cleanup(root)
      } yield assertTrue(
        response.status == Status.BadRequest,
        body.contains("duplicate player names")
      )
    },
    test(
      "POST /api/matches/{id}/handicap rejects out-of-range values with 400"
    ) {
      for {
        (ctx, root) <- withTempTournament(seeded = false)
        _ <- seedTournament(ctx)
        _ <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/matches/wb-1-1/ready", ""))
        response <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/matches/wb-1-1/handicap",
              """{"handicap":6}"""
            )
          )
        body <- response.body.asString
        _ <- cleanup(root)
      } yield assertTrue(
        response.status == Status.BadRequest,
        body.contains("handicap must be at most 5")
      )
    },
    test("POST /api/matches/{id}/result rejects invalid scores with 400") {
      for {
        (ctx, root) <- withTempTournament(seeded = false)
        _ <- seedTournament(ctx)
        _ <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/matches/wb-1-1/ready", ""))
        _ <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/matches/wb-1-1/handicap",
              """{"handicap":3}"""
            )
          )
        _ <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/matches/wb-1-1/start", ""))
        response <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/matches/wb-1-1/result",
              """{"scoreA":999,"scoreB":0}"""
            )
          )
        body <- response.body.asString
        _ <- cleanup(root)
      } yield assertTrue(
        response.status == Status.BadRequest,
        body.contains("winner score must be 7")
      )
    },
    test(
      "POST /api/tournament/complete retries after period write without event append"
    ) {
      val completed = LocalDate.parse("2026-03-15")
      for {
        (ctx, root) <- withTempTournament(seeded = false)
        _ <- seedTournament(ctx)
        seedBody <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostGet("/api/tournament"))
        seedText <- seedBody.body.asString
        seeded <- ZIO.fromEither(seedText.fromJson[TournamentResponse])
        _ <- playAllReadyMatches(ctx, seeded)
        state <- Replay.replayDir(ctx.tournamentDir.get)
        target <- PeriodEmission.write(ctx.dataDir, state, completed)
        beforeModified <- ZIO.attemptBlocking(target.lastModifiedTime)
        response <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/tournament/complete",
              s"""{"completed":"$completed"}"""
            )
          )
        body <- response.body.asString
        completedState <- ZIO.fromEither(body.fromJson[TournamentResponse])
        afterModified <- ZIO.attemptBlocking(target.lastModifiedTime)
        completedEvents <- ZIO.attemptBlocking(
          ctx.tournamentDir.get.list
            .filter(_.name.endsWith("-completed.json"))
            .toList
        )
        replayed <- Replay.replayDir(ctx.tournamentDir.get)
        _ <- cleanup(root)
      } yield assertTrue(
        response.status == Status.Ok,
        completedState.completed,
        completedEvents.size == 1,
        Replay.isComplete(replayed),
        beforeModified == afterModified
      )
    },
    test(
      "POST /api/tournament/complete returns 409 when period file content mismatches"
    ) {
      val completed = LocalDate.parse("2026-03-15")
      for {
        (ctx, root) <- withTempTournament(seeded = false)
        _ <- seedTournament(ctx)
        seedBody <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostGet("/api/tournament"))
        seedText <- seedBody.body.asString
        seeded <- ZIO.fromEither(seedText.fromJson[TournamentResponse])
        _ <- playAllReadyMatches(ctx, seeded)
        target = ctx.dataDir / PeriodEmission.periodFilename(
          "Spring Open",
          completed
        )
        _ <- ZIO.attemptBlocking(
          target.write(
            PeriodWriter.write(
              Period(
                name = "Other Tournament",
                completed = completed,
                matches = List(
                  PeriodMatch(
                    playerA = Player("Alice"),
                    playerB = Player("Bob"),
                    scoreA = 1,
                    scoreB = 0,
                    raceTo = 1,
                    handicapSuggested = 0,
                    handicapApplied = 0
                  )
                )
              )
            )
          )
        )
        response <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/tournament/complete",
              s"""{"completed":"$completed"}"""
            )
          )
        body <- response.body.asString
        completedEvents <- ZIO.attemptBlocking(
          ctx.tournamentDir.get.list
            .filter(_.name.endsWith("-completed.json"))
            .toList
        )
        replayed <- Replay.replayDir(ctx.tournamentDir.get)
        _ <- cleanup(root)
      } yield assertTrue(
        response.status == Status.Conflict,
        body.contains("mismatch"),
        completedEvents.isEmpty,
        !Replay.isComplete(replayed)
      )
    },
    test("ready rejects illegal transition with 400") {
      for {
        (ctx, root) <- withTempTournament(seeded = false)
        _ <- seedTournament(ctx)
        response <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/matches/wb-1-1/ready", ""))
        again <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/matches/wb-1-1/ready", ""))
        _ <- cleanup(root)
      } yield assertTrue(
        response.status == Status.Ok,
        again.status == Status.BadRequest
      )
    },
    test("director routes map stale event seq to 409") {
      for {
        response <- DirectorRoutes
          .directorOnly(
            Request.post("", Body.empty).copy(remoteAddress = loopback)
          )(
            ZIO.fail(EventLog.InvalidSeq(expected = 9, actual = 1))
          )
        body <- response.body.asString
      } yield assertTrue(
        response.status == Status.Conflict,
        body.contains("event seq must be 9"),
        body.contains("retry the request")
      )
    }
  )
}
