package scalaz
package http

/**
 * HTTP general header fields.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.5">RFC 2616 Section 4.5 General Header Fields</a>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
sealed trait GeneralHeader {
  /**
   * Returns a string representation of this entity header.
   */
  val asString: String
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9">§</a>
 */
final case object CacheControl extends GeneralHeader {
  /**
   * A string representation of this general header.
   */
  override val asString = "Cache-Control"
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.10">§</a>
 */
final case object Connection extends GeneralHeader {
  /**
   * A string representation of this general header.
   */
  override val asString = toString
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.18">§</a>
 */
final case object Date extends GeneralHeader {
  /**
   * A string representation of this general header.
   */
  override val asString = toString
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.32">§</a>
 */
final case object Pragma extends GeneralHeader {
  /**
   * A string representation of this general header.
   */
  override val asString = toString
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.40">§</a>
 */
final case object Trailer extends GeneralHeader {
  /**
   * A string representation of this general header.
   */
  override val asString = toString
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.41">§</a>
 */
final case object TransferEncoding extends GeneralHeader {
  /**
   * A string representation of this general header.
   */
  override val asString = "Transfer-Encoding"
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.42">§</a>
 */
final case object Upgrade extends GeneralHeader {
  /**
   * A string representation of this general header.
   */
  override val asString = toString
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.45">§</a>
 */
final case object Via extends GeneralHeader {
  /**
   * A string representation of this general header.
   */
  override val asString = toString
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.46">§</a>
 */
final case object Warning extends GeneralHeader {
  /**
   * A string representation of this general header.
   */
  override val asString = toString
}

trait GeneralHeaders {
  /**
   * Returns a general header for the given list of characters.
   */
  implicit def ListGeneralHeader : (List[Char] => Option[GeneralHeader]) = StringGeneralHeader _ compose (_.mkString)

  /**
   * Returns a string representation for the given general header.
   */
  implicit def GeneralHeaderString(h: GeneralHeader) = h.asString

  /**
   * Returns a general header for the given string.
   */
  implicit def StringGeneralHeader(s: String) = s.toLowerCase match {
    case "cache-control" => Some(CacheControl)
    case "connection" => Some(Connection)
    case "date" => Some(Date)
    case "pragma" => Some(Pragma)
    case "trailer" => Some(Trailer)
    case "transfer-encoding" => Some(TransferEncoding)
    case "upgrade" => Some(Upgrade)
    case "via" => Some(Via)
    case "warning" => Some(Warning)
    case _ => None
  }
}

/**
 * HTTP general header fields.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.5">RFC 2616 Section 4.5 General Header Fields</a>.
 */
object GeneralHeader extends GeneralHeaders {
  /**
   * A list of all general entity headers.
   */
  val generalHeaders = List(CacheControl, Connection, Date, Pragma, Trailer, TransferEncoding,
      Upgrade, Via, Warning)
}
