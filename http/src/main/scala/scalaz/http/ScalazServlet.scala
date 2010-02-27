package scalaz
package http
package servlet

import javax.servlet.{ServletException, ServletConfig}
import Class.forName
import HttpServletRequest._
import HttpServletResponse._

/**
 * A servlet that can run scalaz applications. It is intended that subclasses apply one or more type arguments until a
 * concrete class.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
abstract class ScalazServlet[IN[_], OUT[_], A <: { def application: ServletApplication[IN, OUT] }](cl: Class[A])(implicit in: InputStreamer[IN], e: Each[OUT]) extends javax.servlet.http.HttpServlet {
  /**
   * A servlet application from request to response.
   */
  type App = { def application: ServletApplication[IN, OUT] }

  private[servlet] var application: App = _

  /**
   * The <code>init-param</code> to specify in <code>web.xml</code> that denotes the class that is assignable to the
   * class given in the constructor. It is the application that takes a request to a response.
   */
  val applicationInitParam = "application"

  /**
   * Checks for the existence of the application init-param, that it is of a correct type and instantiates it.
   */
  override final def init(config: ServletConfig) {
    super.init(config)
    val p = config.getInitParameter(applicationInitParam)

    if(p == null)
      throw new ServletException("Specify the " + applicationInitParam + " init parameter")
    else {
      val c = forName(p)

      if(cl isAssignableFrom c) {
        application = c.newInstance.asInstanceOf[App]
      } else
        throw new ServletException(c + " must be assignable to " + cl)
    }
  }

  /**
   * Applies the given request to the underlying web application and sends the resulting response to the given response.
   */
  override final def service(request: javax.servlet.http.HttpServletRequest, response: javax.servlet.http.HttpServletResponse) {
    request.asRequest[IN].foreach(r => {
      val res = application.application(this, request, r)
      response.respond[OUT](res)
    })
  }
}
