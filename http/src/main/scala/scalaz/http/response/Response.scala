package scalaz
package http
package response


import request.Request
import request.UserAgent
import Scalaz._
import Util._
import Util.Nel._

/**
 * HTTP response.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6">RFC 2616 Section 6 Response</a>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
sealed trait Response[OUT[_]] {
  /**
   * The response status line.
   */
  val line: StatusLine

  /**
   * The response headers.
   */
  val headers: List[(ResponseHeader, NonEmptyList[Char])]

  /**
   * The response body.
   */
  val body: OUT[Byte]

  import Response.response

  /**
   * Add the given header and value to this response.
   */
  def apply(h: ResponseHeader, v: NonEmptyList[Char]) = response[OUT](line, (h, v) :: headers, body)

  /**
   * Add the given header and value to this response.
   * <strong>This function fails if the given string value is empty</strong>.
   */
  def apply(h: ResponseHeader, s: String): Response[OUT] = apply(h, s.charsNelErr("Header values must be non-empty"))

  /**
   * Replace the status line of this reponse with the given status line.
   */
  def apply(l: StatusLine) = response[OUT](l, headers, body)

  /**
   * Return the value of given header.
   */
  def apply(h: ResponseHeader) = headersMapHeads.get(h)

  /**
   * Replace the body of this response with the given body.
   */
  def >>(b: OUT[Byte]) = response[OUT](line, headers, b)

  /**
   * Append the given value to the body of this response.
   */
  def |+|(b: OUT[Byte])(implicit s: Semigroup[OUT[Byte]]) = response[OUT](line, headers, s.append(body, b))

  /**
   * Delete the given header value from this response.
   */
  def -(h: ResponseHeader) = response[OUT](line, headers filter { case (k, _) => h != k }, body)

  /**
   * Append the given value to the body of this response.
   */
  def <<[A](a: A)(implicit b: Body[OUT, A], s: Semigroup[OUT[Byte]]) = response[OUT](line, headers, s.append(body, b(a)))

  /**
   * Prepend the given value to the body of this response.
   */
  def <<:[A](a: A)(implicit b: Body[OUT, A], s: Semigroup[OUT[Byte]]) = response[OUT](line, headers, s.append(b(a), body))

  /**
   * A map of headers offering optimal seek time.
   */
  lazy val headersMap = asHashMap[List, NonEmptyList](headers)

  /**
   * A map of headers offering optimal seek time.
   */
  lazy val headersMapHeads = mapHeads(headersMap)

  /**
   * The response version major of the status line.
   */
  def versionMajor = line.version.major

  /**
   * The response version minor of the status line.
   */
  def versionMinor = line.version.minor

  /**
   * The response status.
   */
  def status = line.status

  /**
   * The response status.
   */
  def reasonPhrase = line.reasonPhrase

  /**
   * Returns <code>true</code> if the status code of this response is a 200 OK.
   */
  def isOK = status == OK

  /**
   * Returns <code>true</code> if the status code of this response is a 400 Bad Request.
   */
  def isBadRequest = status == BadRequest

  /**
   * Returns <code>true</code> if the value of the given content-type header satisfies the given condition.
   */
  def contentType(f: NonEmptyList[Char] => Boolean) =
    (this(ContentType): Iterable[NonEmptyList[Char]]) exists f

  /**
   * Returns <code>true</code> if the value of the given content-type header equals the given value.
   */
  def contentTypeEquals(s: String) = contentType(_.mkString == s)

  /**
   * Returns <code>true</code> if this response has set the content-type header.
   */
  def hasContentType = this(ContentType).isDefined

  /**
   * The length of the response body.
   */
  def bodyLength(implicit f: Foldable[OUT]) = body ♯

  /**
   * Returns the response body as a string.
   */
  def bodyString(implicit e: Each[OUT]) = {
    val s = new StringBuilder
    e.each(body, (b: Byte) => s append (b.toChar))
    s.toString
  }

  /**
   * Sets the Content-Type response header for HTML.
   */
  def html = this(ContentType, "text/html")

  /**
   * Sets the Content-Type response header for XHTML.
   */
  def xhtml = this(ContentType, "application/xhtml+xml")

  /**
   * Sets the Content-Type response header for XML.
   */
  def xml = this(ContentType, "text/xml")

  /**
   * Sets the ContentType response header according to the
   * <a href="http://www.w3schools.com/media/media_mimeref.asp">W3C MIME Reference</a> if the given request path
   * has a file extension and corresponds to a known file extension.
   */
  def unary_~[IN[_]](implicit request: Request[IN]) =
    ContentTypeResolver.w3cMimeReference(request.pathExtension) map (this(ContentType, _)) getOrElse this

  /**
   * Sets the content-type response header for XHTML (<code>application/xhtml+xml</code>), however, if the browser
   * identifies itself as not accepting this content type, set the response header for HTML
   * (<code>text/html</code>).
   */
  def acceptsXhtml[IN[_]](implicit req: Request[IN]) =
    if(hasContentType) this else this(ContentType, if(req.isInternetExplorer) "text/html" else "application/xhtml+xml")
}

import request.Request
import StatusLine.statusLine

/**
 * HTTP response.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6">RFC 2616 Section 6 Response</a>.
 */
object Response {
  /**
   * Create a response with the given status line, headers and body.
   */
  def response[OUT[_]](l: StatusLine, h: List[(ResponseHeader, NonEmptyList[Char])], b: OUT[Byte]) =
    new Response[OUT] {
      val line = l
      val headers = h
      val body = b
    }

  // Why can't I name these to 'response' without client code barfing?
  // http://www.nabble.com/Why-does-this-fail-the-compiler--tc19024578.html
  // https://lampsvn.epfl.ch/trac/scala/ticket/1236
  /**
   * Create a response with the given status line and body and no headers.
   */
  def emptyHeadersResponse[OUT[_]](l: StatusLine, b: OUT[Byte]): Response[OUT] = response[OUT](l, Nil, b)

  /**
   * Create a response with the given status line and headers and empty body.
   */
  def emptyResponse[OUT[_]](l: StatusLine, h: List[(ResponseHeader, NonEmptyList[Char])])(implicit e: Empty[OUT]) =
    response[OUT](l, h, e.empty)

  /**
   * Create a response with the given status line and no headers and empty body.
   */
  def emptyHeadersBodyResponse[OUT[_]](l: StatusLine)(implicit e: Empty[OUT]) =
    emptyResponse[OUT](l, Nil)

  /**
   * Create a response with the given status, to give rise to a status line whose version is derived from the given request, and headers and empty body.
   */
  def statusResponse[OUT[_], IN[_]](s: Status, h: List[(ResponseHeader, NonEmptyList[Char])], b: OUT[Byte])(implicit req: Request[IN]) =
    response[OUT](statusLine[IN](s), h, b)

  /**
   * Create a response with the given status, to give rise to a status line whose version is derived from the given request, body and no headers.
   */
  def emptyHeadersStatusResponse[OUT[_], IN[_]](s: Status, b: OUT[Byte])(implicit req: Request[IN]) =
    response[OUT](statusLine[IN](s), Nil, b)

  /**
   * Create a response with the given status, to give rise to a status line whose version is derived from the given request, headers and empty body.
   */
  def emptyStatusResponse[OUT[_], IN[_]](s: Status, h: List[(ResponseHeader, NonEmptyList[Char])])(implicit e: Empty[OUT], req: Request[IN]) =
    response[OUT](statusLine[IN](s), h, e.empty)

  /**
   * Create a response with the given status, to give rise to a status line whose version is derived from the given request, no headers and empty body.
   */
  def emptyHeadersBodyStatusResponse[OUT[_], IN[_]](s: Status)(implicit e: Empty[OUT], req: Request[IN]) =
    response[OUT](statusLine[IN](s), Nil, e.empty)

  /**
   * Create a response with the given version and status in the status line, headers and body.
   */
  def versionStatusResponse[OUT[_]](v: Version, s: Status, h: List[(ResponseHeader, NonEmptyList[Char])], b: OUT[Byte]) =
    response[OUT](statusLine(v, s), h, b)

  /**
   * Create a response with the given version and status in the status line, body and no headers.
   */
  def emptyHeadersVersionStatusResponse[OUT[_]](v: Version, s: Status, b: OUT[Byte]) =
    response[OUT](statusLine(v, s), Nil, b)

  /**
   * Create a response with the given version and status in the status line, headers and empty body.
   */
  def emptyVersionStatusResponse[OUT[_]](v: Version, s: Status, h: List[(ResponseHeader, NonEmptyList[Char])])(implicit e: Empty[OUT]) =
    response[OUT](statusLine(v, s), h, e.empty)

  /**
   * Create a response with the given version and status in the status line, no headers and empty body.
   */
  def emptyHeadersBodyVersionStatusResponse[OUT[_]](v: Version, s: Status)(implicit e: Empty[OUT]) =
    response[OUT](statusLine(v, s), Nil, e.empty)

  /**
   * Create a response with the given version that redirects (301 Moved Permanently) to the given location.
   */
  def versionRedirect[OUT[_]](version: Version, location: NonEmptyList[Char])(implicit e: Empty[OUT]) =
    response[OUT](statusLine(version, MovedPermanently), List((Location, location)), e.empty)

  /**
   * Create a response with the given version that redirects (301 Moved Permanently) to the given location.
   * <strong>This function fails if the given string value is empty</strong>.
   */
  def versionRedirects[OUT[_]](version: Version, location: String)(implicit e: Empty[OUT]) =
    response[OUT](statusLine(version, MovedPermanently), List((Location, location.charsNelErr("location must be non-empty"))), e.empty)

  /**
   * Create a response with a version derived from the given request that redirects (301 Moved Permanently) to the given location.
   */
  def redirect[OUT[_], IN[_]](location: NonEmptyList[Char], parameters: (String, String)*)(implicit e: Empty[OUT], req: Request[IN]) =
    response[OUT](statusLine[IN](MovedPermanently), List((Location, if(parameters.isEmpty) location else location :::> ('?' + encode(parameters: _*)).toList)), e.empty)

  /**
   * Create a response with a version derived from the given request that redirects (301 Moved Permanently) to the given location.
   * <strong>This function fails if the given string value is empty</strong>.
   */
  def redirects[OUT[_], IN[_]](location: String, parameters: (String, String)*)(implicit e: Empty[OUT], req: Request[IN]) =
    redirect[OUT, IN](location.charsNelErr("location must be non-empty"), parameters: _*)
}
