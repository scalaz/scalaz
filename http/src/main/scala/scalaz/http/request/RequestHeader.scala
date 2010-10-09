package scalaz
package http
package request

/**
 * HTTP request headers.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14">RFC 2616 Section 14 Header Field Definitions</a>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
sealed trait RequestHeader {
  /**
   * A string representation of this request header.
   */
  val asString: String

  /**
   * Returns <code>true</code> if this header is an <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.1">entity header</a>,
   * <code>false</code> otherwise.
   */
  lazy val isEntity = this match {
    case Entity(_) => true
    case _ => false
  }

  /**
   * Returns <code>true</code> if this header is a <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.5">general header</a>,
   * <code>false</code> otherwise.
   */
  lazy val isGeneral = this match {
    case General(_) => true
    case _ => false
  }

  /**
   * Returns the result of the given function to this header if it is an
   * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.1">entity header</a>, otherwise returns the given value.
   */
  def entity[X](f: EntityHeader => X, x: => X) = this match {
    case Entity(h) => f(h)
    case _ => x
  }

  /**
   * Returns the result of the given function to this header if it is a
   * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.5">general header</a>, otherwise returns the given value.
   */
  def general[X](f: GeneralHeader => X, x: => X) = this match {
    case General(h) => f(h)
    case _ => x
  }
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.1">§</a>
 */
final case object Accept extends RequestHeader {
  override val asString = toString
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.2">§</a>
 */
final case object AcceptCharset extends RequestHeader {
  override val asString = "Accept-Charset"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.3">§</a>
 */
final case object AcceptEncoding extends RequestHeader {
  override val asString = "Accept-Encoding"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.4">§</a>
 */
final case object AcceptLanguage extends RequestHeader {
  override val asString = "Accept-Language"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.8">§</a>
 */
final case object Authorization extends RequestHeader {
  override val asString = toString
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.20">§</a>
 */
final case object Expect extends RequestHeader {
  override val asString = toString
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.22">§</a>
 */
final case object From extends RequestHeader {
  override val asString = toString
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.23">§</a>
 */
final case object Host extends RequestHeader {
  override val asString = toString
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.24">§</a>
 */
final case object IfMatch extends RequestHeader {
  override val asString = "If-Match"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.25">§</a>
 */
final case object IfModifiedSince extends RequestHeader {
  override val asString = "If-Modified-Since"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.26">§</a>
 */
final case object IfNoneMatch extends RequestHeader {
  override val asString = "If-None-Match"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.27">§</a>
 */
final case object IfRange extends RequestHeader {
  override val asString = "If-Range"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.28">§</a>
 */
final case object IfUnmodifiedSince extends RequestHeader {
  override val asString = "If-Unmodified-Since"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.31">§</a>
 */
final case object MaxForwards extends RequestHeader {
  override val asString = "Max-Forwards"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.34">§</a>
 */
final case object ProxyAuthorization extends RequestHeader {
  override val asString = "Proxy-Authorization"
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35">§</a>
 */
final case object Range extends RequestHeader {
  override val asString = toString
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.36">§</a>
 */
final case object Referer extends RequestHeader {
  override val asString = toString
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.39">§</a>
 */
final case object TE extends RequestHeader {
  override val asString = toString
}

/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.43">§</a>
 */
final case object UserAgent extends RequestHeader {
  override val asString = "User-Agent"
}
private final case class Entity(eh: EntityHeader) extends RequestHeader {
  override val asString = eh.asString
}
private final case class General(gh: GeneralHeader) extends RequestHeader {
  override val asString = gh.asString
}

import Character.isWhitespace
import Scalaz._
import Util.Nel._

trait RequestHeaders {
  /**
   * Converts the given entity header into a request header.
   */
  implicit def entityToRequest(eh: EntityHeader): RequestHeader = scalaz.http.request.Entity(eh)

  /**
   * Converts the given general header into a request header.
   */
  implicit def generalToRequest(gh: GeneralHeader): RequestHeader = scalaz.http.request.General(gh)

  /**
   * Converts the given string to a request header. If the string is a known request header, then it is used. If not,
   * then it if it is a known general header, then it is used. If not then it is an entity header.
   */
  implicit def StringRequestHeader(s: String): Option[RequestHeader] =
    RequestHeader.headers find {case (n, h) => n.equalsIgnoreCase(s) } map (_._2) orElse
            (s: Option[GeneralHeader]) ∘ (scalaz.http.request.General(_)) orElse
            (s: Option[EntityHeader]) ∘ (scalaz.http.request.Entity(_))

  /**
   * Converts the given list of characters to a request header. If the string is a known request header, then it is
   * used. If not, then it if it is a known general header, then it is used. If not then it is an entity header.
   */
  implicit def ListRequestHeader: (List[Char] => Option[RequestHeader]) = StringRequestHeader _ compose (_.mkString)
}

/**
 * HTTP request headers.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14">RFC 2616 Section 14 Header Field Definitions</a>.
 */
object RequestHeader extends RequestHeaders {
  /**
   * For deconstructing request headers into entity headers.
   */
  object Entity {
    /**
     * Matches if the given request header is an entity header.
     */
    def unapply(h: RequestHeader) = h match {
      case scalaz.http.request.Entity(x) => Some(x)
      case _ => None
    }
  }

  /**
   * For deconstructing request headers into general headers.
   */
  object General {
    /**
     * Matches if the given request header is a general header.
     */
    def unapply(h: RequestHeader) = h match {
      case scalaz.http.request.General(x) => Some(x)
      case _ => None
    }
  }

  import GeneralHeader.StringGeneralHeader
  import EntityHeader.StringEntityHeader

  /**
   * A list of known headers.
   */
  val headers = List(("accept", Accept),
    ("accept-charset", AcceptCharset),
    ("accept-encoding", AcceptEncoding),
    ("accept-language", AcceptLanguage),
    ("authorization", Authorization),
    ("from", From),
    ("host", Host),
    ("if-match", IfMatch),
    ("if-modified-since", IfModifiedSince),
    ("if-none-match", IfNoneMatch),
    ("if-range", IfRange),
    ("if-unmodified-since", IfUnmodifiedSince),
    ("max-forwards", MaxForwards),
    ("proxy-authorization", ProxyAuthorization),
    ("range", Range),
    ("referer", Referer),
    ("te", TE),
    ("user-agent", UserAgent))

  /**
   * Converts a list of characters of the form "abc:def" into a potential request header and non-empty value split at
   * the colon (:).
   */
  def requestHeaderValue(cs: List[Char]): Option[(RequestHeader, NonEmptyList[Char])] =
    cs span (_ != ':') match {
      case (n, v) => {
        (n: Option[RequestHeader]) ∗ (h =>
          (v.dropWhile(x => x == ':' || isWhitespace(x))).toNel map (v => (h, v)))
      }
    }
}
