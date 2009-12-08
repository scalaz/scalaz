package scalaz.http

import request.{GET, Request}
import scalaz.Scalaz._
import response.xhtml.Doctype.transitional
import response.{Response, NotFound, OK}
import servlet.{HttpServletRequest, ServletApplication, HttpServlet, StreamStreamServletApplication}
import servlet.HttpServlet._
import response.Response._
import response.Body._
import StreamStreamApplication._

final class ExampleApplication extends StreamStreamServletApplication {
  implicit val charset = UTF8

  def hello(implicit request: Request[Stream]) = OK(ContentType, "text/html") << transitional << {
    <html>
      <body>
        <p>Hello World</p>
      </body>
    </html>
  }

  def handle(implicit request: Request[Stream], servletRequest: HttpServletRequest): Option[Response[Stream]] = {
    request match {
      case Request.MethodPath(GET, "/") => Some(hello)
      case _ => None
    }
  }

  val application = new ServletApplication[Stream, Stream] {
    def application(implicit servlet: HttpServlet, servletRequest: HttpServletRequest, request: Request[Stream]) = {
      handle getOrElse resource(x => OK << x.toStream, NotFound.xhtml)
    }
  }
}
