package scalaz
package http
package servlet

import Scalaz._

/**
 * A wrapper around Java Servlet <code>HttpSession</code>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
sealed trait HttpSession {
  /**
   * The wrapped HTTP session.
   */
  val session: javax.servlet.http.HttpSession

  /**
   * Returns the attribute associated with the given value.
   */
  def apply(attr: String) = Option(session.getAttribute(attr))
  
  /**
   * Returns the attribute associated with the given value. If no attribute is
   * associated with the value, a new association is made with the given 
   * <tt>default</tt> value, and that value is returned.
   * 
   * @throws ClassCastException if the attribute exists and is not an instance 
   *         of <tt>A</tt>.
   */
  def getOrAdd[A](attr: String, default: =>A) = 
    (apply(attr) ∘ ((v : Object) => v.asInstanceOf[A])) | {
      update(attr, default)
      default
    }

  /**
   * Deletes the attribute associated with the given value.
   */
  def -=(attr: String) = session.removeAttribute(attr)

  /**
   * Sets the given attribute name to the given value.
   */
  def update[A](attr: String, value: A) = session.setAttribute(attr, value)
}

/**
 * A wrapper around Java Servlet <code>HttpSession</code>.
 */
trait HttpSessions {
  /**
   * Wraps the given HTTP session.
   */
  implicit def HttpSessionSession(s: javax.servlet.http.HttpSession): HttpSession = new HttpSession {
    val session = s
  }

  /**
   * Unwraps the given HTTP session into a servlet session.
   */
  implicit def SessionHttpSession(session: HttpSession) = session.session
}

object HttpSession extends HttpSessions
