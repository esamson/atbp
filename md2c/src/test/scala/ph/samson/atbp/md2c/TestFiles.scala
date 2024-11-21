package ph.samson.atbp.md2c

import better.files.*

object TestFiles {

  def apply(name: String): File = File(Resource.getUrl(name))

}
