package ph.samson.atbp.cli

import zio.ZIO

trait ToolCommand {
  def run(conf: Conf): ZIO[Any, Throwable, Unit]
}
