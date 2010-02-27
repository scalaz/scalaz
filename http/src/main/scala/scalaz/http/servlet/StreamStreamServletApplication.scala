package scalaz
package http
package servlet

import request.Request

/**
 * A servlet web application with a request body and response body made up of a stream of bytes.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
trait StreamStreamServletApplication {
  /**
   * The servlet web application.
   */
  val application: ServletApplication[Stream, Stream]
}

import Scalaz._
import response.Response
import servlet.HttpServlet._
import response.OK
import response.StreamResponse.{response, statusLine}

/**
 * A servlet web application with a request body and response body made up of a stream of bytes.
 */
object StreamStreamServletApplication {
  /**
   * Constructs a stream/stream servlet web application from the given argument.
   */
  def application(a: ServletApplication[Stream, Stream]) = new StreamStreamServletApplication {
    val application = a
  }

  /**
   * Handles a request in such a way that if the given function produces no response then return the given response.
   */
  implicit def resourceOr(f: Request[Stream] => Option[Response[Stream]]) = new {
    def or(otherwise: Request[Stream] => Response[Stream]): ServletApplication[Stream, Stream] =
      new ServletApplication[Stream, Stream] {
      def application(implicit servlet: HttpServlet, servletRequest: HttpServletRequest, request: Request[Stream]) =
        f(request) | (request.path ? (in => response(statusLine(OK), in.toStream), otherwise(request)))
    }
  }
}
