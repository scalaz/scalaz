package scalaz
package http
package servlet

import Scalaz._
import Util.Nel._

/**
 * A wrapper around Java Servlet <code>HttpServlet</code>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
sealed trait HttpServlet {
  /**
   * The wrapped HTTP servlet.
   */
  val servlet: javax.servlet.http.HttpServlet

  /**
   * Returns a potential resource loaded with using the servlet container. 
   */
  def resource(path: String) = Option(servlet.getServletContext.getResourceAsStream(path)) map (x => x)
}

import request.Request

trait HttpServlets {
  /**
   * Wraps the given Java Servlet.
   */
  implicit def HttpServletServlet(s: javax.servlet.http.HttpServlet): HttpServlet = new HttpServlet {
    val servlet = s
  }

  /**
   * Unwraps the given HTTP servlet into a servlet.
   */
  implicit def ServletHttpServlet(s: HttpServlet) = s.servlet


  /**
   * Loads a resource at the given path. If that resource is found, return the result of applying the given function,
   * otherwise return the given value.
   */
  implicit def Resource(path: NonEmptyList[Char]): { def ?[A](found: Iterator[Byte] => A, notFound: => A)(implicit s: HttpServlet): A } = new {
    def ?[A](found: Iterator[Byte] => A, notFound: => A)(implicit s: HttpServlet) =
      s.resource(path) map (z => found(z.elements)) getOrElse notFound
  }
}
/**
 * A wrapper around Java Servlet <code>HttpServlet</code>.
 */
object HttpServlet extends HttpServlets {
  /**
   * Loads a resource at the given path. If that resource is found, return the result of applying the given function,
   * otherwise return the given value.
   */
  def resource[A](path: String, found: Iterator[Byte] => A, notFound: => A)(implicit s: HttpServlet) =
    s.resource(path) map (z => found(z.elements)) getOrElse notFound

  /**
   * Loads a resource at the path of the given request. If that resource is found, return the result of applying the
   * given function, otherwise return the given value.
   */
  def resource[IN[_], A](found: Iterator[Byte] => A, notFound: => A)(implicit s: HttpServlet, request: Request[IN]): A =
    resource(request.path.mkString, found, notFound)

}
