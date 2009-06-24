package scalaz.http.request

import Util.Nel._

/**
 * HTTP request method.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9">RFC 2616 Section 9 Method Definitions</a>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Method {
  /**
   * A string representation of this request method.
   */
  val asString: String

  /**
   * A non-empty string representation of this request method.
   */
  val asNonEmptyList: NonEmptyList[Char] = toString.toList.get

  /**
   * Returns <code>true</code> if this method is an extension method, <code>false</code> otherwise.
   */
  val isExtension = this match {
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
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.2">§</a>
 */
final case object OPTIONS extends Method {
  override val asString = toString
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.3">§</a>
 */
final case object GET extends Method {
  override val asString = toString
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.4">§</a>
 */
final case object HEAD extends Method {
  override val asString = toString
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.5">§</a>
 */
final case object POST extends Method {
  override val asString = toString
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.6">§</a>
 */
final case object PUT extends Method {
  override val asString = toString
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.7">§</a>
 */
final case object DELETE extends Method {
  override val asString = toString
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.8">§</a>
 */
final case object TRACE extends Method {
  override val asString = toString
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.9">§</a>
 */
final case object CONNECT extends Method {
  override val asString = toString
}
private final case class ExtensionMethod(m: NonEmptyList[Char]) extends Method {
  override val asString = m.mkString
}

/**
 * HTTP request method.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9">RFC 2616 Section 9 Method Definitions</a>.
 */
object Method {
  /**
   * A list of known methods.
   */
  val methods = List(OPTIONS,GET,HEAD,POST,PUT,DELETE,TRACE,CONNECT)

  /**
   * An extractor that always matches with a non-empty string representation of this request method.
   */
  def unapply(m: Method) = Some(m.asNonEmptyList)

  /**
   * Converts the given non-empty string representation into a request method. If it is a known request method then that
   * is used, otherwise an extension method is returned.
   */
  implicit def NonEmptyListMethod(s: NonEmptyList[Char]): Method = StringMethod(List.toString(s)).get

  /**
   * Converts the given string representation into a request method. If it is a known request method then that
   * is used, otherwise an extension method is returned.
   */
  implicit def ListMethod = StringMethod _ compose List.toString

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
      val t = (s: scala.runtime.RichString).toList
      ExtensionMethod(NonEmptyList.nel(t.head, t.tail))
    }
  })
}