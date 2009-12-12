package scalaz
package http

import Scalaz._
import java.lang.Character.isDigit

/**
 * HTTP version.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.1">RFC 2616 Section 3.1 HTTP Version</a>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision < br >
 *          $LastChangedDate: 2009-06-24 20:48:22 +1000 (Wed, 24 Jun 2009) $<br>
 *          $LastChangedBy: tonymorris $
 */
sealed trait Version {
  /**
   * HTTP Version major number.
   */
  val major: Digit

  /**
   * HTTP Version major number.
   */
  val minor: Digit

  /**
   * A string representation of this version
   */
  def asString = "HTTP/" + major.toInt + "." + minor.toInt
}

trait Versions {
  /**
   * Returns a string representation for the given version.
   */
  implicit def VersionString(v: Version) = v.asString

  /**
   * Returns a string representation for the given version.
   */
  implicit def ListVersion: (List[Char] => Option[Version]) = StringVersion _ compose (_.mkString)

  /**
   * Returns a potential version for the given string of the form <code>HTTP/major/minor</code>.
   */
  implicit def StringVersion(s: String): Option[Version] =
    if (s.length < 8)
      None
    else {
      val major = s charAt 5
      val minor = s charAt 7

      // Without the following line, we get this error when compiling using SBT (with CompileOption("-Xlog-implicits") :: CompileOption("-Ydebug")):
      //      scalaz.this.Applicative.applicative is not a valid implicit value for scalaz.this.Applicative[scala.this.Option] because:
      //polymorphic expression cannot be instantiated to expected type;
      // found   : [<deferred> <param> Z[<deferred> <param> _]]scalaz.this.Applicative[Z]
      // required: scalaz.this.Applicative[scala.this.Option]
      //[error] E:\code\scalaz\http\src\main\scala\scalaz\http\Version.scala:85: could not find implicit value for parameter a: scalaz.Applicative[Option]
      //[error]       List(major, minor) ? ((c: Char) => isDigit(c).option(c.toLong - 48L)) map { case List(maj, min) => version(maj, min) }
      //
      // TODO boil the problem down and update https://lampsvn.epfl.ch/trac/scala/ticket/2778
      implicit val a = Applicative.applicative[Option]

      List(major, minor) ↦ ((c: Char) => isDigit(c).option(c.toLong - 48L)) map {case List(maj, min) => Version.version(maj, min)}
    }
}

/**
 * HTTP version.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.1">RFC 2616 Section 3.1 HTTP Version</a>.
 */
object Version extends Versions {
  /**
   * Extracts the major and minor numbers of the given version.
   */
  def unapply(v: Version): Option[(Digit, Digit)] =
    Some(v.major, v.minor)

  /**
   * Constructs a version with the given major and minor numbers.
   */
  def version(maj: Digit, min: Digit): Version = new Version {
    val major = maj
    val minor = min
  }

  /**
   * A version for HTTP/1.0.
   */
  val version10 = version(_1, _0)

  /**
   * A version for HTTP/1.1.
   */
  val version11 = version(_1, _1)
}
