package scalaz
package http
package response

import Scalaz._

/**
 * HTTP response status line.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.1">RFC 2616 Section 6.1 Status-Line</a>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
sealed trait StatusLine {
  /**
   * The HTTP version in the status line. This is typically HTTP 1.0 or 1.1.
   */
  val version: Version

  /**
   * The response status code.
   */
  val status: Status

  /**
   * The reason phrase for the status code.
   */
  val reasonPhrase: List[Char]
}

import request.Request
import Version.version
import Scalaz._

/**
 * HTTP response status line.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.1">RFC 2616 Section 6.1 Status-Line</a>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
object StatusLine {
  /**
   * Create a status line with the given version, status and reason phrase.
   */
  def statusLine(v: Version, s: Status, r: List[Char]) = new StatusLine {
    val version = v
    val status = s
    val reasonPhrase = r
  }

  /**
   * Create a status line with the given version, status and a reason phrase derived from the status if it has one
   * associated. <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.1.1">RFC 2616 Section 6.1.1 Status Code and Reason Phrase</a>.
   */
  def statusLine(version: Version, status: Status): StatusLine = statusLine(version, status, status.reasonPhrase | Nil)

  /**
   * Create a status line with the given status, a version derived from the given request and a reason phrase
   * derived from the status if it has one associated.
   * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.1.1">RFC 2616 Section 6.1.1 Status Code and Reason Phrase</a>.
   */
  def statusLine[IN[_]](status: Status)(implicit req: Request[IN]): StatusLine = statusLine(req.line.version, status)

  /**
   * Create a status line with the given status, a HTTP/1.0 version and a reason phrase derived from the status
   * if it has one associated.
   * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.1.1">RFC 2616 Section 6.1.1 Status Code and Reason Phrase</a>.
   */
  import Digit._
  
  implicit def statusLine10(status: Status) = statusLine(version(_1, _0), status)

  /**
   * Create a status line with the given status, a HTTP/1.1 version and a reason phrase derived from the status
   * if it has one associated.
   * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.1.1">RFC 2616 Section 6.1.1 Status Code and Reason Phrase</a>.
   */
  implicit def statusLine11(status: Status) = statusLine(version(_1, _1), status)
}
