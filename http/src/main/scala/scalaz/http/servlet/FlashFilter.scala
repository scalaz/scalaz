package scalaz
package http
package servlet

import javax.servlet.{ServletRequest, ServletResponse, Filter, FilterChain, FilterConfig}
import HttpServletRequest._

/**
 * A filter that will keep a session variable until the next request where its value is placed in a request attribute.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
final class FlashFilter extends Filter {
  /**
   * No-op.
   */
  override def init(config : FilterConfig) = {}

  /**
   * No-op.
   */
  override def destroy = {}

  import FlashFilter.flashParameter

  /**
   * If there is a session parameter set then unset it and put its value in a request attribute.
   */
  override def doFilter(request : ServletRequest, response : ServletResponse, chain : FilterChain) {
    val r = request.asInstanceOf[javax.servlet.http.HttpServletRequest]

    r.session(flashParameter) foreach (flash => {
      r(flashParameter) = flash
      r.session -= (flashParameter)
    })

    chain.doFilter(request, response)
  }
}

/**
 * A filter that will keep a session variable until the next request where its value is placed in a request attribute.
 */
object FlashFilter {
  /**
   * The session and request attribute used for associating a value.
   */
  val flashParameter = "scalaz.http.servlet.FlashFilter.flashParameter"

  /**
   * Sets the value to be used in the next request using the session.
   */
  def flashSet[A](v: A)(implicit r: HttpServletRequest) {
    r.session(flashParameter) = v 
  }

  /**
   * Gets the value from the request that was previously set if there is one. 
   */
  def flashGet[A](implicit r: HttpServletRequest) = r attr flashParameter map (_.asInstanceOf[A])
}
