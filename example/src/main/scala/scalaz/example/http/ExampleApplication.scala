package scalaz.example
package http

import scalaz._
import Scalaz._
import scalaz.http._
import response._
import request._
import Slinky._
import servlet._
import HttpServlet.resource

final class ExampleApplication extends StreamStreamServletApplication {
  implicit val charset = UTF8

  import Request._

  def ok(s: String)(implicit request: Request[Stream]) = OK(ContentType, "text/html") << transitional << say(s)

  def say(s: String) = {
    <html>
      <body>
        <p>
          {s}
        </p>
      </body>
    </html>
  }

  def handle(implicit request: Request[Stream], servletRequest: HttpServletRequest): Option[Response[Stream]] = {
    request match {
      case MethodParts(GET, Nil) => Some(ok("Hello World"))
      case MethodParts(GET, "hello" :: name :: Nil) => Some(ok("Hello, %s" format (name)))
      case _ => None
    }
  }

  val application = new ServletApplication[Stream, Stream] {
    def application(implicit servlet: HttpServlet, servletRequest: HttpServletRequest, request: Request[Stream]) = {
      handle | resource(x => OK << x.toStream, NotFound.xhtml)
    }
  }
}
