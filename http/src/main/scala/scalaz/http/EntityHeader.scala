package scalaz
package http

import http.Util.Nel._
import Scalaz._

/**
 * HTTP entity header fields.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.1">RFC 2616 Section 7.1 Entity Header Fields</a>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
sealed trait EntityHeader {
  /**
   * Returns a string representation of this entity header.
   */
  val asString: String

  /**
   * Returns <code>true</code> if this entity header is an extension header.
   */
  lazy val isExtension = this match {
    case ExtensionHeader(_) => true
    case _ => false
  }

  /**
   * Returns the result of the given function to this header if it is an extension header, otherwise returns the given
   * value.
   */
  def extension[X](f: NonEmptyList[Char] => X, x: => X) = this match {
    case ExtensionHeader(h) => f(h)
    case _ => x
  }
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.7">§</a>
 */
final case object Allow extends EntityHeader {
  /**
   * A string representation of this entity header.
   */
  override val asString = toString
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11">§</a>
 */
final case object ContentEncoding extends EntityHeader {
  /**
   * A string representation of this entity header.
   */
  override val asString = "Content-Encoding"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.12">§</a>
 */
final case object ContentLanguage extends EntityHeader {
  /**
   * A string representation of this entity header.
   */
  override val asString = "Content-Language"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13">§</a>
 */
final case object ContentLength extends EntityHeader {
  /**
   * A string representation of this entity header.
   */
  override val asString = "Content-Length"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.14">§</a>
 */
final case object ContentLocation extends EntityHeader {
  /**
   * A string representation of this entity header.
   */
  override val asString = "Content-Location"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.15">§</a>
 */
final case object ContentMD5 extends EntityHeader {
  /**
   * A string representation of this entity header.
   */
  override val asString = "Content-MD5"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.16">§</a>
 */
final case object ContentRange extends EntityHeader {
  /**
   * A string representation of this entity header.
   */
  override val asString = "Content-Range"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17">§</a>
 */
final case object ContentType extends EntityHeader {
  /**
   * A string representation of this entity header.
   */
  override val asString = "Content-Type"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21">§</a>
 */
final case object Expires extends EntityHeader {
  /**
   * A string representation of this entity header.
   */
  override val asString = toString
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.29">§</a>
 */
final case object LastModified extends EntityHeader {
  /**
   * A string representation of this entity header.
   */
  override val asString = "Last-Modified"
}

private final case class ExtensionHeader(name: NonEmptyList[Char]) extends EntityHeader {
  override val asString = name.list.mkString
}

trait EntityHeaders {
  /**
   * Returns a string representation for the given entity header.
   */
  implicit def EntityHeaderString(h: EntityHeader) = h.asString

  /**
   * Returns a potential entity header for the given list of characters; <code>None</code> iff the given list is empty.
   */
  implicit def ListEntityHeader: (List[Char] => Option[EntityHeader]) = StringEntityHeader _ compose (_.mkString)

  /**
   * Returns a potential entity header for the given string; <code>None</code> iff the given string is empty.
   */
  implicit def StringEntityHeader(s: String): Option[EntityHeader] = if (s.length == 0) None else Some(s.toLowerCase match {
    case "allow" => Allow
    case "content-encoding" => ContentEncoding
    case "content-language" => ContentLanguage
    case "content-length" => ContentLength
    case "content-location" => ContentLocation
    case "content-md5" => ContentMD5
    case "content-range" => ContentRange
    case "content-type" => ContentType
    case "expires" => Expires
    case "last-modified" => LastModified
    case h => {
      val t: List[Char] = (s: scala.collection.immutable.StringOps).toList
      ExtensionHeader(nel(t.head, t.tail))
    }
  })
}

/**
 * HTTP entity header fields.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.1">RFC 2616 Section 7.1 Entity Header Fields</a>.
 */
object EntityHeader extends EntityHeaders {
  /**
   * A list of all non-extension entity headers.
   */
  val entityHeaders = List(Allow, ContentEncoding, ContentLanguage, ContentLength, ContentLocation,
    ContentMD5, ContentRange, ContentType, Expires, LastModified)

  /**
   * Extracts the given entity header into a string representation.
   */
  def unapply(h: EntityHeader) = Some(h.asString)
}