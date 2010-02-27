package scalaz
package http
package servlet

import request.Request
import Scalaz._
import request.{Method, RequestHeader}
import request.Line.line
import request.Uri.uri
import HttpSession.HttpSessionSession
import Util.Nel._

/**
 * A wrapper around Java Servlet <code>HttpServletRequest</code>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
sealed trait HttpServletRequest {
  /**
   * The wrapped HTTP servlet request.
   */
  val request: javax.servlet.http.HttpServletRequest

  /**
   * Returns the request parameter value for the given argument.
   */
  def apply(param: String) = Option(request.getParameter(param))

  /**
   * Returns the request parameter value for the given argument.
   * <strong>This function fails if the request has no such parameter</strong>.
   */
  def !(param: String) = Option(request.getParameter(param)) err ("Missing request parameter: " + param)

  /**
   * Removes the given request attribute.
   */
  def -=(attr: String) = request.removeAttribute(attr)

  /**
   * Sets the given request attribute with the given value.
   */
  def update[A](attr: String, value: A) = request.setAttribute(attr, value)

  /**
   * Gets the given request attribute value.
   */
  def attr(attr: String) = Option(request.getAttribute(attr))

  /**
   * Returns the HTTP session associated with this request.
   */
  def session = HttpSessionSession(request.getSession)

  /**
   * Returns the HTTP session associated with this request.
   */
  def session(create: Boolean) = HttpSessionSession(request.getSession(create))

  /**
   * Converts this request into a scalaz request.
   */
  def asRequest[I[_]](implicit in: InputStreamer[I]) = {
      val headers: List[(RequestHeader, NonEmptyList[Char])] = request.getHeaderNames.elements.map(_.asInstanceOf[String]).toList ∗
              (h => request.getHeaders(h).elements.map(_.asInstanceOf[String]).filter(_.length > 0).map
                        (v => ((h: Option[RequestHeader]).get, v.toList.toNel.get)).toList)

      val rline = (request.getMethod.toList: Option[Method]) >>= (m =>
        request.getRequestURI.toList.toNel map
                (p => uri(p, Option(request.getQueryString) map (_.toList))) >>=
                (u => (request.getProtocol: Option[Version]) map
                        (v => line(m, u, v))))

      rline map (Request.request[I](_, headers, in(request.getInputStream)))
    }
}

trait HttpServletRequests {
  /**
   * Wraps the given Java Servlet HTTP request.
   */
  implicit def HttpServletRequestRequest(r: javax.servlet.http.HttpServletRequest): HttpServletRequest = new HttpServletRequest {
    val request = r
  }

  /**
   * Unwraps the given HTTP request into a servlet HTTP request.
   */
  implicit def RequestHttpServletRequest(request: HttpServletRequest) = request.request
}

/**
 * A wrapper around Java Servlet <code>HttpServletRequest</code>.
 */
object HttpServletRequest extends HttpServletRequests {
  /**
   * Prepends the context path of the given HTTP servlet request to the given argument. This is useful for creating
   * anchor tags in HTML documents.
   */
  def link(s: String)(implicit request: HttpServletRequest) =
    request.getContextPath + '/' + s

  /**
   * Removes the length of the context path of the given servlet request unless it is empty.
   */
  def c[IN[_]](r: Request[IN])(implicit request: HttpServletRequest) = {
    val k: Option[NonEmptyList[Char]] = (r.path drop request.getContextPath.length).toNel
    k ∘ (p => r(r.uri(p))) | r
  }

  object MethodPath {
    def unapply[IN[_]](r: Request[IN])(implicit hsr: HttpServletRequest): Option[(Method, String)] =
      scalaz.http.request.Request.MethodPath.unapply(r) map (ms => (ms._1, ms._2.drop((hsr.getContextPath + "/").length)))
  }

  object Path {
    def unapply[IN[_]](r: Request[IN])(implicit hsr: HttpServletRequest): Option[String] =
      scalaz.http.request.Request.Path.unapply(r) map (_.drop((hsr.getContextPath + "/").length))
  }
}
