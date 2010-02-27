package scalaz
package http
package response

import Scalaz._
import Util.Digits.{digitsLong, longDigits}

/**
 * HTTP response status codes.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10">RFC 2616 Section 10 Status Code Definitions</a>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
sealed trait Status {
  /**
   * An integer representation of the response status.
   */
  val toInt: Int

  /**
   * A three-digit representation of the response status.
   */
  lazy val digits: (Digit, Digit, Digit) = {
    val x = longDigits[List](toInt)
    (x(0), x(1), x(2))
  }

  /**
   * A <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.1.1">reason phrase</a> associated with the
   * response status (if there is one).
   */
  def reasonPhraseS: Option[String]

  /**
   * A <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.1.1">reason phrase</a> associated with the
   * response status (if there is one).
   */
  def reasonPhrase = reasonPhraseS map (_.toList)

  /**
   * <code>true</code> if this status code is an extension-code, <code>false</code> otherwise.
   */
  val isExtension = this match {
    case ExtensionCode(_, _, _) => true
    case _ => false
  }

  /**
   * Executes the given function if this is an extension-code, otherwise returns the given value.
   */
  def extension[X](f: (Digit, Digit, Digit) => X, x: => X) = this match {
    case ExtensionCode(a, b, c) => f(a, b, c)
    case _ => x
  }
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.1.1">§</a>
 */
case object Continue extends Status {
  override val toInt = 100
  override val reasonPhraseS = Some("Continue")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.1.2">§</a>
 */
case object SwitchingProtocols extends Status {
  override val toInt = 101
  override val reasonPhraseS = Some("Switching Protocols")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.1">§</a>
 */
case object OK extends Status {
  override val toInt = 200
  override val reasonPhraseS = Some("OK")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.2">§</a>
 */
case object Created extends Status {
  override val toInt = 201
  override val reasonPhraseS = Some("Created")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.3">§</a>
 */
case object Accepted extends Status {
  override val toInt = 202
  override val reasonPhraseS = Some("Accepted")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.4">§</a>
 */
case object NonAuthoritativeInformation extends Status {
  override val toInt = 203
  override val reasonPhraseS = Some("Non-Authoritative Information")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.5">§</a>
 */
case object NoContent extends Status {
  override val toInt = 204
  override val reasonPhraseS = Some("No Content")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.6">§</a>
 */
case object ResetContent extends Status {
  override val toInt = 205
  override val reasonPhraseS = Some("Reset Content")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.7">§</a>
 */
case object PartialContent extends Status {
  override val toInt = 206
  override val reasonPhraseS = Some("Partial Content")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.1">§</a>
 */
case object MultipleChoices extends Status {
  override val toInt = 300
  override val reasonPhraseS = Some("Multiple Choices")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.2">§</a>
 */
case object MovedPermanently extends Status {
  override val toInt = 301
  override val reasonPhraseS = Some("Moved Permanently")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.3">§</a>
 */
case object Found extends Status {
  override val toInt = 302
  override val reasonPhraseS = Some("Found")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.4">§</a>
 */
case object SeeOther extends Status {
  override val toInt = 303
  override val reasonPhraseS = Some("See Other")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.5">§</a>
 */
case object NotModified extends Status {
  override val toInt = 304
  override val reasonPhraseS = Some("Not Modified")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.6">§</a>
 */
case object UseProxy extends Status {
  override val toInt = 305
  override val reasonPhraseS = Some("Use Proxy")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.8">§</a>
 */
case object TemporaryRedirect extends Status {
  override val toInt = 307
  override val reasonPhraseS = Some("Temporary Redirect")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.1">§</a>
 */
case object BadRequest extends Status {
  override val toInt = 400
  override val reasonPhraseS = Some("Bad Request")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.2">§</a>
 */
case object Unauthorized extends Status {
  override val toInt = 401
  override val reasonPhraseS = Some("Unauthorized")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.3">§</a>
 */
case object PaymentRequired extends Status {
  override val toInt = 402
  override val reasonPhraseS = Some("Payment Required")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.4">§</a>
 */
case object Forbidden extends Status {
  override val toInt = 403
  override val reasonPhraseS = Some("Forbidden")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.5">§</a>
 */
case object NotFound extends Status {
  override val toInt = 404
  override val reasonPhraseS = Some("Not Found")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.6">§</a>
 */
case object MethodNotAllowed extends Status {
  override val toInt = 405
  override val reasonPhraseS = Some("Method Not Allowed")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.7">§</a>
 */
case object NotAcceptable extends Status {
  override val toInt = 406
  override val reasonPhraseS = Some("Not Acceptable")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.8">§</a>
 */
case object ProxyAuthenticationRequired extends Status {
  override val toInt = 407
  override val reasonPhraseS = Some("Proxy Authentication Required")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.9">§</a>
 */
case object RequestTimeout extends Status {
  override val toInt = 408
  override val reasonPhraseS = Some("Request Time-out")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.10">§</a>
 */
case object Conflict extends Status {
  override val toInt = 409
  override val reasonPhraseS = Some("Conflict")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.11">§</a>
 */
case object Gone extends Status {
  override val toInt = 410
  override val reasonPhraseS = Some("Gone")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.12">§</a>
 */
case object LengthRequired extends Status {
  override val toInt = 411
  override val reasonPhraseS = Some("Length Required")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.13">§</a>
 */
case object PreconditionFailed extends Status {
  override val toInt = 412
  override val reasonPhraseS = Some("Precondition Failed")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.14">§</a>
 */
case object RequestEntityTooLarge extends Status {
  override val toInt = 413
  override val reasonPhraseS = Some("Request Entity Too Large")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.15">§</a>
 */
case object RequestURITooLong extends Status {
  override val toInt = 414
  override val reasonPhraseS = Some("Request-URI Too Large")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.16">§</a>
 */
case object UnsupportedMediaType extends Status {
  override val toInt = 415
  override val reasonPhraseS = Some("Unsupported Media Type")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.17">§</a>
 */
case object RequestedRangeNotSatisfiable extends Status {
  override val toInt = 416
  override val reasonPhraseS = Some("Requested range not satisfiable")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.18">§</a>
 */
case object ExpectationFailed extends Status {
  override val toInt = 417
  override val reasonPhraseS = Some("Expectation Failed")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.5.1">§</a>
 */
case object InternalServerError extends Status {
  override val toInt = 500
  override val reasonPhraseS = Some("Internal Server Error")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.5.2">§</a>
 */
case object NotImplemented extends Status {
  override val toInt = 501
  override val reasonPhraseS = Some("Not Implemented")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.5.3">§</a>
 */
case object BadGateway extends Status {
  override val toInt = 502
  override val reasonPhraseS = Some("Bad Gateway")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.5.4">§</a>
 */
case object ServiceUnavailable extends Status {
  override val toInt = 503
  override val reasonPhraseS = Some("Service Unavailable")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.5.5">§</a>
 */
case object GatewayTimeout extends Status {
  override val toInt = 504
  override val reasonPhraseS = Some("Gateway Time-out")
}
/**
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.5.6">§</a>
 */
case object HTTPVersionNotSupported extends Status {
  override val toInt = 505
  override val reasonPhraseS = Some("HTTP Version not supported")
}
private final case class ExtensionCode(a: Digit, b: Digit, c: Digit) extends Status {
  override val toInt = digitsLong[List](List(a, b, c)).toInt
  override val reasonPhraseS = None
}

/**
 * HTTP response status codes.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10">RFC 2616 Section 10 Status Code Definitions</a>.
 */
object Status {
  /**
   * Construct a request status code with the given three digits.
   */
  def status(d1: Digit, d2: Digit, d3: Digit): Status =
    digitsLong[List](List(d1, d2, d3)).toInt.get

  /**
   * An integer representation of the given status code.
   */
  implicit def StatusInt(s: Status): Int = s.toInt

  /**
   * List of known statuses.
   */
  val statuses =
      List(Continue, SwitchingProtocols, OK, Created, Accepted, NonAuthoritativeInformation,
          NoContent, ResetContent, PartialContent, MultipleChoices, MovedPermanently, Found, SeeOther,
          NotModified, UseProxy, TemporaryRedirect, BadRequest, Unauthorized, PaymentRequired, Forbidden,
          NotFound, MethodNotAllowed, NotAcceptable, ProxyAuthenticationRequired, RequestTimeout,
          Conflict, Gone, LengthRequired, PreconditionFailed, RequestEntityTooLarge, RequestURITooLong,
          UnsupportedMediaType, RequestedRangeNotSatisfiable, ExpectationFailed, InternalServerError,
          NotImplemented, BadGateway, ServiceUnavailable, GatewayTimeout, HTTPVersionNotSupported)

  /**
   * Returns a status code for integer values between 100 and 999 inclusive.
   */
  implicit def IntStatus(n: Int): Option[Status] = n match {
    case 100 => Some(Continue)
    case 101 => Some(SwitchingProtocols)
    case 200 => Some(OK)
    case 201 => Some(Created)
    case 202 => Some(Accepted)
    case 203 => Some(NonAuthoritativeInformation)
    case 204 => Some(NoContent)
    case 205 => Some(ResetContent)
    case 206 => Some(PartialContent)
    case 300 => Some(MultipleChoices)
    case 301 => Some(MovedPermanently)
    case 302 => Some(Found)
    case 303 => Some(SeeOther)
    case 304 => Some(NotModified)
    case 305 => Some(UseProxy)
    case 307 => Some(TemporaryRedirect)
    case 400 => Some(BadRequest)
    case 401 => Some(Unauthorized)
    case 402 => Some(PaymentRequired)
    case 403 => Some(Forbidden)
    case 404 => Some(NotFound)
    case 405 => Some(MethodNotAllowed)
    case 406 => Some(NotAcceptable)
    case 407 => Some(ProxyAuthenticationRequired)
    case 408 => Some(RequestTimeout)
    case 409 => Some(Conflict)
    case 410 => Some(Gone)
    case 411 => Some(LengthRequired)
    case 412 => Some(PreconditionFailed)
    case 413 => Some(RequestEntityTooLarge)
    case 414 => Some(RequestURITooLong)
    case 415 => Some(UnsupportedMediaType)
    case 416 => Some(RequestedRangeNotSatisfiable)
    case 417 => Some(ExpectationFailed)
    case 500 => Some(InternalServerError)
    case 501 => Some(NotImplemented)
    case 502 => Some(BadGateway)
    case 503 => Some(ServiceUnavailable)
    case 504 => Some(GatewayTimeout)
    case 505 => Some(HTTPVersionNotSupported)
    case _ => (n >= 0 && n <= 999).option(ExtensionCode(n / 100, n % 100 / 10, n % 10))
  }
}
