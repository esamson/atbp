package ph.samson.atbp.stmt2csv.parsers

import better.files.File
import ph.samson.atbp.stmt2csv.CsvEntry
import ph.samson.xdg.basedir.*
import zio.internal.stacktracer.SourceLocation
import zio.test.*

abstract class ParserIntegrationTest(parser: StatementParser)
    extends ZIOSpecDefault {
  import ParserIntegrationTest.*

  def parserTest(statementFileName: String)(
      assertion: List[CsvEntry] => TestResult
  ): Spec[Any, Throwable] = {
    TestDir match {
      case Some(testDir) =>
        val file = testDir / statementFileName
        test(s"parse $statementFileName") {
          for {
            entries <- parser(file)
          } yield assertion(entries)
        }
      case None =>
        test(s"skip $statementFileName") {
          assertTrue(true)
        } @@ TestAspect.ifPropSet(Path)
    }
  }
}

/** Parser tests rely on bank statements stored somewhere else.
  *
  * Set the system property `stmt2csv.test.dir` to point to the directory
  * containing the bank statements.
  *
  * You can also set the configuration in your
  * `~/.config/atbp/application.conf`.
  */
object ParserIntegrationTest {

  val Path = "stmt2csv.test.dir"
  val TestDir: Option[File] = {
    val conf = config("atbp").load
    if (conf.hasPath(Path)) {
      Some(File(conf.getString(Path)))
    } else {
      None
    }
  }
}
