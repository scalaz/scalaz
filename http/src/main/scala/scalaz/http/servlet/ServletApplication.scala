package scalaz.http.servlet

import scalaz.http.response.Response
import scala.collection.mutable.HashMap
import scalaz.http.request.Request
import scalaz._
import scalaz.http.InputStreamer

import scalaz.http.Application
import scala.{Application => _}

/**
 * A HTTP servlet application.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision<br>
 *          $LastChangedDate: 2009-06-24 20:48:22 +1000 (Wed, 24 Jun 2009) $<br>
 *          $LastChangedBy: tonymorris $
 */
abstract class ServletApplication[IN[_], OUT[_]](implicit i: InputStreamer[IN], e: Each[OUT]) {
  /**
   * Returns a response for the given application parts.
   */
  def application(implicit servlet: HttpServlet, servletRequest: HttpServletRequest, request: Request[IN]) : Response[OUT]

  /**
   * Returns a web application for the given servlet application parts.
   */
  def application(implicit servlet: HttpServlet, servletRequest: HttpServletRequest) : Application[IN, OUT] = new Application[IN, OUT] {
    def apply(implicit request: Request[IN]) = application(servlet, servletRequest, request)
  }

  /**
   * Returns a response for the given application parts.
   */
  def apply(servlet: HttpServlet, servletRequest: HttpServletRequest, req: Request[IN]) = application(servlet, servletRequest)(req)

  /**
   * The input-streamer for the contents of the request body.
   */
  val inputStreamer = i

  /**
   * The each for the contents of the response body.
   */
  val each = e   
}

/**
 * A HTTP servlet application.
 */
object ServletApplication {
  /**
   * Construct a servlet application from the given function.
   */
  def servletApplication[IN[_], OUT[_]](f: (HttpServlet, HttpServletRequest, Request[IN]) => Response[OUT])(implicit i: InputStreamer[IN], e: Each[OUT]) =
    new ServletApplication[IN, OUT] {
      def application(implicit servlet: HttpServlet, servletRequest: HttpServletRequest, request: Request[IN]) = f(servlet, servletRequest, request)
    }

  /**
   * Construct a servlet application from the given function.
   */
  def servletApplication_[IN[_], OUT[_]](f: (HttpServlet, HttpServletRequest) => Application[IN, OUT])(implicit i: InputStreamer[IN], e: Each[OUT]) =
    new ServletApplication[IN, OUT] {
      def application(implicit servlet: HttpServlet, servletRequest: HttpServletRequest, request: Request[IN]) = f(servlet, servletRequest)(request)
    }

  /**
   * Construct a servlet application from the given function.
   */
  def servletApplication__[IN[_], OUT[_]](f: (HttpServlet) => Application[IN, OUT])(implicit i: InputStreamer[IN], e: Each[OUT]) =
    servletApplication_[IN, OUT]((s, r) => f(s))

  /**
   * Construct a servlet application from the given function.
   */
  def servletApplication___[IN[_], OUT[_]](f: (HttpServletRequest) => Application[IN, OUT])(implicit i: InputStreamer[IN], e: Each[OUT]) =
    servletApplication_[IN, OUT]((s, r) => f(r))

  /**
   * Construct a servlet application from the given constant value.
   */
  def application[IN[_], OUT[_]](a: Application[IN, OUT])(implicit i: InputStreamer[IN], e: Each[OUT]) =
    servletApplication_[IN, OUT]((s, r) => a)
}
