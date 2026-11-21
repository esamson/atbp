package ph.samson.atbp.md2c

import java.net.URI
import java.net.URISyntaxException
import java.util.Locale

object CloneUrl {
  private[md2c] def selectRemote(names: List[String]): Option[String] =
    if (names.contains("origin")) Some("origin")
    else names match { case name :: Nil => Some(name); case _ => None }

  private val SshSchemes = Set("ssh", "git+ssh", "ssh+git")
  private val NetworkSchemes = SshSchemes ++ Set("git", "http", "https")
  private val SchemeLikeHosts =
    NetworkSchemes ++ Set("file", "ftp", "ftps", "rsync")
  private val SshUser = "[A-Za-z0-9_][A-Za-z0-9._-]*".r
  private val ScpLocation = """([^/:\[\]]*(?:\[[^\[\]]+\])?):([^:].*)""".r
  private val DrivePrefix = "^[A-Za-z]:".r
  private val EncodedQueryOrFragment = "(?i)%(?:25)*(?:23|26|3f)".r

  private def parseUri(value: String): Option[URI] =
    try Some(new URI(value))
    catch { case _: URISyntaxException => None }

  private def validAuthority(uri: URI, allowUser: Boolean): Boolean =
    Option(uri.getHost).nonEmpty &&
      !uri.getRawAuthority.endsWith(":") &&
      uri.getPort != 0 && uri.getPort <= 65535 &&
      Option(uri.getRawUserInfo).forall(user =>
        allowUser && SshUser.matches(user)
      )

  private def supportedCloneLocation(url: String): Boolean =
    if (url.contains("://"))
      parseUri(url).exists(uri =>
        NetworkSchemes(uri.getScheme) &&
          validAuthority(uri, SshSchemes(uri.getScheme)) &&
          uri.getPath.nonEmpty && !uri.getPath.exists(_.isControl)
      )
    else
      url match {
        case ScpLocation(authority, path) =>
          // Reject forms ambiguous with a malformed scheme or user:password@host.
          !SchemeLikeHosts(authority.toLowerCase(Locale.ROOT)) &&
          !path.takeWhile(_ != '/').contains("@") &&
          parseUri(s"ssh://$authority").exists(
            validAuthority(_, allowUser = true)
          )
        case _ => false // Relative and local paths only work on this machine.
      }

  private[md2c] def safeCloneCommand(url: String): Option[String] = {
    val valid = url.nonEmpty && url == url.strip &&
      !url.exists(c => c.isControl || c == '?' || c == '#' || c == '\\') &&
      DrivePrefix.findFirstIn(url).isEmpty &&
      EncodedQueryOrFragment.findFirstIn(url).isEmpty &&
      supportedCloneLocation(url)
    Option.when(valid)(s"git clone -- '${url.replace("'", "'\"'\"'")}'")
  }
}
