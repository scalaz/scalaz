package scalaz
package http
package request

import Util.Nel._
import Scalaz._

/**
 * A request line.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1">RFC 2616 Section 5.1 Request-Line</a>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
sealed trait Line {
  /**
   * The <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1.1">request method</a>.
   */
  val method: Method

  /**
   * The <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1.2">request URI</a>.
   */
  val uri: Uri

  /**
   * The <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.1">request version</a>.
   */
  val version: Version

  import Line.line

  /**
   * Returns a request line with the given method and this URI and version.
   */
  def apply(m: Method): Line = line(m, uri, version)

  /**
   * Returns a request line with the given URI and this method and version.
   */
  def apply(u: Uri): Line = line(method, u, version)
  
  /**
   * Returns a request line with the given version and this method and URI.
   */
  def apply(v: Version): Line = line(method, uri, v)

  /**
   *  Returns the first occurrence of the given request parameter in the request URI.
   */
  def !(p: String) = uri.parametersMapHeads flatMap (_.get(p.toList))

  /**
   * Returns the first occurrence of the given request parameter in the request URI or the given error value.
   */
  def ![E](p: String, e: => E): Validation[E, List[Char]] = this ! p toSuccess e

  /**
   * Returns all occurrences of the given request parameter in the request URI.
   */
  def !!(p: String) = OptionNonEmptyListList(uri.parametersMap flatMap (_.get(p.toList)))

  /**
   * Returns all occurrences of the given request parameter in the request URI or the given error value.
   */
  def !![E](p: String, e: => E): Validation[E, NonEmptyList[List[Char]]] = (this !! p).toNel toSuccess e

  /**
   * Returns <code>true</code> if the given request parameter occurs in the request URI.
   */
  def !?(p: String) = this ! p isDefined

  /**
   * Returns <code>false</code> if the given request parameter occurs in the request URI.
   */
  def ~!?(p: String) = this ! p isEmpty
}

trait Lines {
  import Character.isWhitespace
  import Scalaz._

  /**
   * Converts the given string into a potential request line.
   */
  implicit def ListLine(cs: List[Char]): Option[Line] = {
    def reverseTrim(c: List[Char]) = c.dropWhile(isWhitespace(_)).reverse.dropWhile(isWhitespace(_))
    val x = cs span (!isWhitespace(_))
    val m: Option[Method] = x._1
    val y = x._2.reverse span (!isWhitespace(_))
    val u: Option[Uri] = reverseTrim(y._2)
    val v: Option[Version] = reverseTrim(y._1)
    v <*> (u <*> (m map (m => u => v => Line.line(m, u, v))))
  }
}
/**
 * A request line.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1">RFC 2616 Section 5.1 Request-Line</a>.
 */
object Line extends Lines {
  /**
   * An extractor that always matches with the method, URI and version of the given request line.
   */
  def unapply(line: Line): Option[(Method, Uri, Version)] =
    Some(line.method, line.uri, line.version)

  /**
   * Construct a request line with the given method, URI and version
   */
  def line(m: Method, u: Uri, v: Version): Line = new Line {
    val method = m
    val uri = u
    val version = v
  }
}
