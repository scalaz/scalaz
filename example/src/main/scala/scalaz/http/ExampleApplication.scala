package scalaz
package http

import Scalaz._
import response._
import request._
import Slinky._
import servlet._
import HttpServlet.resource

final class ExampleApplication extends StreamStreamServletApplication {
  implicit val charset = UTF8

  import Request._

  // todo added the explicit calls to Slinky.response when migrating the Scala 2.8.0 Beta1 RC8. It seems that
  // there is a name shadowing occurring otherwise between the package response and the method response.
  // Boil it down and report a bug. See https://lampsvn.epfl.ch/trac/scala/changeset/20486
  def ok(s: String)(implicit request: Request[Stream]) = Slinky.response(OK).apply(ContentType, "text/html") << transitional << say(s)

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
      handle getOrElse resource(x => Slinky.response(OK) << x.toStream, Slinky.response(NotFound).xhtml)
    }
  }
}
