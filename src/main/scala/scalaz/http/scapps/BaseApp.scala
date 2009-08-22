package scalaz.http.scapps

import java.util.Date
import scalaz.Scalaz._
import scalaz.http.request.{Method, Request}
import scalaz.http.servlet.{HttpServlet, HttpServletRequest, ServletApplication, StreamStreamServletApplication}, HttpServlet._, HttpServletRequest._
import scalaz.http.StreamStreamApplication._
import scalaz.http.{Application}
import scalaz.http.response._
import scalaz.http.response.Body._

/**
 * Base application that provides Optional handling of request. Looks up resources if no routes match, then 404.
*/
abstract class BaseApp extends StreamStreamServletApplication {
  def route(implicit request : Request[Stream], servletRequest: HttpServletRequest) : Option[Response[Stream]]

  def log(time: Date, servletRequest: HttpServletRequest, request: Request[Stream], response: Response[Stream]) {
    println("""%s - - [%s] "%s %s %s" %s 0""".format(servletRequest.getRemoteAddr, new Date(), request.method.asString, request.path.list.mkString, request.version.asString, response.status.toInt.toString))
    println(request.body.map(_.toChar).mkString)
  }

  val application = new ServletApplication[Stream, Stream] {
    def application(implicit servlet: HttpServlet, servletRequest: HttpServletRequest, request: Request[Stream]) = {
      val time = new Date()
      val response = route getOrElse resource(x => OK << Stream.fromIterator(x), NotFound.xhtml)
      log(time, servletRequest, request, response)
      response
    }
  }
}

