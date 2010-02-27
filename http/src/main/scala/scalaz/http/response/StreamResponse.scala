package scalaz
package http
package response

import request.Request

/**
 * Functions to create responses that have a stream body.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
object StreamResponse {
  /**
   * Create a response with the given status line, headers and body.
   */
  def response(l: StatusLine, h: List[(ResponseHeader, NonEmptyList[Char])], b: Stream[Byte]) =
    Response.response[Stream](l, h, b)

  /**
   * Create a response with the given status line and body with no headers.
   */
  def response(l: StatusLine, b: Stream[Byte]): Response[Stream] =
    Response.emptyHeadersResponse[Stream](l, b)

  /**
   * Create a response with the given status line and headers and empty body.
   */
  def response(l: StatusLine, h: List[(ResponseHeader, NonEmptyList[Char])]): Response[Stream] =
    Response.emptyResponse[Stream](l, h)

  /**
   * Create a response with the given status line, no headers and empty body.
   */
  implicit def response(l: StatusLine): Response[Stream] =
    Response.emptyHeadersBodyResponse[Stream](l)

  /**
   * Create a response with the given version, status, headers and body.
   */
  def response(v: Version, s: Status, h: List[(ResponseHeader, NonEmptyList[Char])], b: Stream[Byte]): Response[Stream] =
    Response.versionStatusResponse[Stream](v, s, h, b)

  /**
   * Create a response with the given version, status, body and no headers.
   */
  def response(v: Version, s: Status, b: Stream[Byte]): Response[Stream] =
    Response.emptyHeadersVersionStatusResponse[Stream](v, s, b)

  /**
   * Create a response with the given version, status, headers and empty body.
   */
  def response(v: Version, s: Status, h: List[(ResponseHeader, NonEmptyList[Char])]): Response[Stream] =
    Response.emptyVersionStatusResponse[Stream](v, s, h)

  /**
   * Create a response with the given version and status, no headers and empty body.
   */
  def response(v: Version, s: Status): Response[Stream] =
    Response.emptyHeadersBodyVersionStatusResponse[Stream](v, s)

  /**
   * Create a response with the given version that redirects (301 Moved Permanently) to the given location.
   */
  def redirect(v: Version, location: NonEmptyList[Char]) = Response.versionRedirect[Stream](v, location)

  /**
   * Create a response with the given version that redirects (301 Moved Permanently) to the given location.
   * <strong>This function fails if the given string value is empty</strong>.
   */
  def redirect(v: Version, location: String) = Response.versionRedirects[Stream](v, location)

  /**
   * Create a response with the given status, a version derived from the given request, no headers and an empty body.
   */
  def statusLine(status: Status)(implicit req: Request[Stream]) = StatusLine.statusLine[Stream](status)

  /**
   * Create a response with the given status, a HTTP/1.0 version, no headers and an empty body.
   */
  implicit def response10(s: Status): Response[Stream] =
    response(StatusLine.statusLine10(s), Nil)

  /**
   * Create a response with the given status, a HTTP/1.1 version, no headers and an empty body.
   */
  implicit def response11(s: Status): Response[Stream] =
    response(StatusLine.statusLine11(s), Nil)
}
