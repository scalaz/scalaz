package scalaz
package http
package servlet

import javax.servlet.http.Cookie
import response.Response
import Util.Nel._
import Scalaz._

/**
 * A wrapper around Java Servlet <code>HttpServletResponse</code>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
sealed trait HttpServletResponse {
  /**
   * The wrapped HTTP servlet response.
   */
  val response: javax.servlet.http.HttpServletResponse

  /**
   * Sets the given header to the given value on this response.
   */
  def update(header: String, value: String) = response.setHeader(header, value)

  /**
   * Sets the given header to the given values on this response.
   */
  def update[V[_]](header: String, value: V[String])(implicit v: Each[V]) = v.each[String](value, response.addHeader(header, _))

  /**
   * Sets the given header to the given value on this response.
   */
  def update(header: String, value: Int) = response.setIntHeader(header, value)

  /**
   * Sets the given header to the given value on this response.
   */
  def update(header: String, value: Long) = response.setDateHeader(header, value)

  /**
   * Sets the given header to the given cookie value on this response.
   */
  def update(header: String, cookie: Cookie) = response.addCookie(cookie)

  /**
   * Side-effects against this HTTP servlet response using the given response.
   */
  def respond[OUT[_]](res: Response[OUT])(implicit e: Each[OUT]) {
    response.setStatus(res.line.status)

    res.headers.foreach { case (h, v) => response.setHeader(h, v.mkString) }

    val out = response.getOutputStream
    e.each[Byte](res.body, out.write(_))
  }
}

/**
 * A wrapper around Java Servlet <code>HttpServletResponse</code>.
 */
trait HttpServletResponses {
  /**
   * Wraps the given Java Servlet HTTP response.
   */
  implicit def HttpServletResponseResponse(r: javax.servlet.http.HttpServletResponse): HttpServletResponse = new HttpServletResponse {
    val response = r
  }

  /**
   * Unwraps the given HTTP response into a servlet HTTP response.
   */
  implicit def ResponseHttpServletResponse(response: HttpServletResponse) = response.response
}

object HttpServletResponse extends HttpServletResponses