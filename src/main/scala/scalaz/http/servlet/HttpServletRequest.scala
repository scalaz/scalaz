package scalaz.http.servlet

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
 * @version $LastChangedRevision<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait HttpServletRequest {
  /**
   * The wrapped HTTP servlet request.
   */
  val request: javax.servlet.http.HttpServletRequest

  /**
   * Returns the request parameter value for the given argument.
   */
  def apply(param: String) = request.getParameter(param).onull

  /**
   * Returns the request parameter value for the given argument.
   * <strong>This function fails if the request has no such parameter</strong>.
   */
  def !(param: String) = request.getParameter(param).onull err ("Missing request parameter: " + param)

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
  def attr(attr: String) = request.getAttribute(attr).onull

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
      val headers: List[(RequestHeader, NonEmptyList[Char])] = request.getHeaderNames.elements.map(_.asInstanceOf[String]).toList >>=
              (h => request.getHeaders(h).elements.map(_.asInstanceOf[String]).filter(_.length > 0).map
                        (v => ((h: Option[RequestHeader]).get, (v.toList: Option[NonEmptyList[Char]]).get)).toList)

      val rline = (request.getMethod.toList: Option[Method]) >>= (m =>
        (request.getRequestURI.toList: Option[NonEmptyList[Char]]) map
                (p => uri(p, request.getQueryString.onull map (_.toList))) >>=
                (u => (request.getProtocol: Option[Version]) map
                        (v => line(m, u, v))))

      rline map (Request.request[I](_, headers, in(request.getInputStream)))
    }
}

/**
 * A wrapper around Java Servlet <code>HttpServletRequest</code>.
 */
object HttpServletRequest {
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
    val k: Option[NonEmptyList[Char]] = r.path drop request.getContextPath.length
    k |> (p => r(r.uri(p))) | r
  }

  object MethodPath {
    def unapply[IN[_]](r: Request[IN])(implicit hsr: HttpServletRequest): Option[(Method, String)] =
      request.Request.MethodPath.unapply(r) map (ms => (ms._1, ms._2.drop((hsr.getContextPath + "/").length)))
  }

  object Path {
    def unapply[IN[_]](r: Request[IN])(implicit hsr: HttpServletRequest): Option[String] =
      request.Request.Path.unapply(r) map (_.drop((hsr.getContextPath + "/").length))
  }
}
