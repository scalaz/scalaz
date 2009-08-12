package scalaz.http.scapps

// TODO clean up these imports if possible

import javax.servlet.ServletContext
import scalaz.Scalaz._
import scalaz.http.request.{Method, Request}
import scalaz.http.servlet.{HttpServlet, HttpServletRequest, ServletApplication, StreamStreamServletApplication}
import scalaz.http.servlet.HttpServlet._
import scalaz.http.StreamStreamApplication._
import scalaz.http.{Application}
import scalaz.http.response._
import scalaz.http.response.Body._

/*
Slinky base application that provides Optional handling of request.
Looks up resources if None.
*/
abstract class BaseApp extends StreamStreamServletApplication {
  def route(implicit request : Request[Stream], servletRequest: HttpServletRequest) : Option[Response[Stream]]

  def log(request: Request[Stream], response: Response[Stream]) {
    //"0.0.0.0 - localhost [%s] \"%s %s HTTP/1.0\" %s 0000" format ("", request.method.asString, request.path.list.mkString, response.status.toInt.toString)
    //request
    // TODO use apache log format
    // 127.0.0.1 - frank [10/Oct/2000:13:55:36 -0700] "GET /apache_pb.gif HTTP/1.0" 200 2326
  }
  
  val application = new ServletApplication[Stream, Stream] {
    def application(implicit servlet: HttpServlet, servletRequest: HttpServletRequest, request: Request[Stream]) = {
      val response = route getOrElse resource(x => OK << Stream.fromIterator(x), NotFound.xhtml)
      log(request, response)
      response
    }
  }
}

