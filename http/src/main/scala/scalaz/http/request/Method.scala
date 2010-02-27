package scalaz
package http
package request

import Util.Nel._
import Scalaz._

/**
 * HTTP request method.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9">RFC 2616 Section 9 Method Definitions</a>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
sealed trait Method {
  /**
   * A string representation of this request method.
   */
  val asString: String

  /**
   * A non-empty string representation of this request method.
   */
  lazy val asNonEmptyList: NonEmptyList[Char] = asString.charsNel.get

  /**
   * Returns <code>true</code> if this method is an extension method, <code>false</code> otherwise.
   */
  lazy val isExtension = this match {
    case ExtensionMethod(_) => true
    case _ => false
  }

  /**
   * Returns the result of the given function to this method if it is an extension method, otherwise returns the given
   * value.
   */
  def extension[X](f: NonEmptyList[Char] => X, x: => X) = this match {
    case ExtensionMethod(m) => f(m)
    case _ => x
  }
}

/**
 * A HTTP method whose name comes from toString().
 */
trait CaseMethod extends Method {
  override val asString = this.toString
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.2">§</a>
 */
case object OPTIONS extends CaseMethod

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.3">§</a>
 */
case object GET extends CaseMethod
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.4">§</a>
 */
case object HEAD extends CaseMethod
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.5">§</a>
 */
case object POST extends CaseMethod
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.6">§</a>
 */
case object PUT extends CaseMethod
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.7">§</a>
 */
case object DELETE extends CaseMethod
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.8">§</a>
 */
case object TRACE extends CaseMethod
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.9">§</a>
 */
case object CONNECT extends CaseMethod

private final case class ExtensionMethod(m: NonEmptyList[Char]) extends Method {
  val asString = m.mkString
}

trait Methods {
  /**
   * Converts the given non-empty string representation into a request method. If it is a known request method then that
   * is used, otherwise an extension method is returned.
   */
  implicit def NonEmptyListMethod(s: NonEmptyList[Char]): Method = StringMethod(s.mkString).get

  /**
   * Converts the given string representation into a request method. If it is a known request method then that
   * is used, otherwise an extension method is returned.
   */
  implicit def ListMethod : (List[Char] => Option[Method]) = StringMethod _ compose (_.mkString)

  /**
   * Returns a string representation of the given request method.
   */
  implicit def MethodString(m: Method) = m.asString

  /**
   * Converts the given non-empty string representation into a request method. If it is a known request method then that
   * is used, otherwise an extension method is returned.
   */
  implicit def StringMethod(s: String): Option[Method] = if(s.length == 0) None else Some(s.toLowerCase match {
    case "options" => OPTIONS
    case "get" => GET
    case "head" => HEAD
    case "post" => POST
    case "put" => PUT
    case "delete" => DELETE
    case "trace" => TRACE
    case "connect" => CONNECT
    case m => {
      val t : List[Char] = (s : scala.collection.immutable.StringOps).toList
      ExtensionMethod(nel(t.head, t.tail))
    }
  })
}

/**
 * HTTP request method.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9">RFC 2616 Section 9 Method Definitions</a>.
 */
object Method extends Methods {
  /**
   * A list of known methods.
   */
  val methods = List(OPTIONS,GET,HEAD,POST,PUT,DELETE,TRACE,CONNECT)

  /**
   * An extractor that always matches with a non-empty string representation of this request method.
   */
  def unapply(m: Method) = Some(m.asNonEmptyList)
}
