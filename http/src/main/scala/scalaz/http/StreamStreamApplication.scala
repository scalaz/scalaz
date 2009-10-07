package scalaz.http

import request.Request
import response.Response
import response.{ResponseHeader, Status}

/**
 * Functions for web applications whose request and response body and transformed using <code>scala.Stream</code>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision<br>
 *          $LastChangedDate: 2009-06-24 20:48:22 +1000 (Wed, 24 Jun 2009) $<br>
 *          $LastChangedBy: tonymorris $
 */
object StreamStreamApplication {
  /**
   * Construct a response from the given status, headers and body.
   */
  def response(s: Status, h: List[(ResponseHeader, NonEmptyList[Char])], b: Stream[Byte])(implicit req: Request[Stream]) =
    Response.statusResponse[Stream, Stream](s, h, b)

  /**
   * Construct a response from the given status and body.
   */
  def response(s: Status, b: Stream[Byte])(implicit req: Request[Stream]) =
    Response.emptyHeadersStatusResponse[Stream, Stream](s, b)

  /**
   * Construct a response from the given status and headers.
   */
  def response(s: Status, h: List[(ResponseHeader, NonEmptyList[Char])])(implicit req: Request[Stream]) =
    Response.emptyStatusResponse[Stream, Stream](s, h)

  /**
   * Construct a response from the given status with a version determined by the given request.
   */
  implicit def response(s: Status)(implicit req: Request[Stream]): Response[Stream] =
    Response.emptyHeadersBodyStatusResponse[Stream, Stream](s)

  /**
   * Create a response with a version derived from the given request that redirects (301 Moved Permanently) to the given location.
   */
  def redirect(location: NonEmptyList[Char], parameters: (String, String)*)(implicit req: Request[Stream]) = Response.redirect[Stream, Stream](location, parameters: _*)

  /**
   * Create a response with a version derived from the given request that redirects (301 Moved Permanently) to the given location.
   * <strong>This function fails if the given string value is empty</strong>.
   */
  def redirect(location: String, parameters: (String, String)*)(implicit req: Request[Stream]) = Response.redirects[Stream, Stream](location, parameters: _*)
}
