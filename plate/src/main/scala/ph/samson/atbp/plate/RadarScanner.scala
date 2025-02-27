package ph.samson.atbp.plate

import better.files.File
import ph.samson.atbp.jira.Client
import ph.samson.atbp.plate.Inspector.JiraLink
import zio.{Task, ZIO, ZLayer}

trait RadarScanner {

  def scan(source: File, excludeProjects: List[String]): Task[File]
}

object RadarScanner {

  private val Suffix = ".radar"

  private class LiveImpl(client: Client) extends RadarScanner {

  def scan(source: File, excludeProjects: List[String]): Task[File] = ZIO.logSpan("scan") {
    for {
      sourceLines <- ZIO.attemptBlockingIO(source.lines.toList)
      outputLines <- doScan(sourceLines, excludeProjects)
      _ <- ZIO.logInfo(s"scan $source, excluding $excludeProjects")
      outFile <- ZIO.attemptBlockingIO {
        val name = source.`extension`() match {
          case Some(ext) =>
            source.nameWithoutExtension(includeAll = false) + Suffix + ext
          case None => source.name + Suffix
        }
        val out = source.sibling(name)
        out.overwrite(outputLines.mkString("\n"))
      }
    } yield outFile
  }

  def doScan(sourceLines: List[String], excludeProjects: List[String]): Task[List[String]] = {
    val plateKeys = sourceLines.collect {
      case JiraLink(key) => key
    }
    val projects = plateKeys.map(key => key.substring(0, key.indexOf('-'))).toSet.filterNot(excludeProjects.contains).toList

    for {
      _ <- ZIO.logInfo(s"keys:\n  ${plateKeys.sorted.mkString("\n  ")}")
      _ <- ZIO.logInfo(s"projects:\n  ${projects.sorted.mkString("\n  ")}")
    } yield Nil
  }

  }

  def layer() = ZLayer {
    for {
      client <- ZIO.service[Client]
    } yield LiveImpl(client): RadarScanner
  }
}
