package scalaz.http.servlet

import scalaz.http.request.Request

/**
 * A servlet web application with a request body and response body made up of a stream of bytes.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision<br>
 *          $LastChangedDate: 2009-06-24 20:48:22 +1000 (Wed, 24 Jun 2009) $<br>
 *          $LastChangedBy: tonymorris $
 */
trait StreamStreamServletApplication {
  /**
   * The servlet web application.
   */
  val application: ServletApplication[Stream, Stream]
}

import scalaz.Scalaz._
import scalaz.http.response.Response
import scalaz.http.servlet.HttpServlet._
import scalaz.http.response.OK
import scalaz.http.response.StreamResponse.{response, statusLine}


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
