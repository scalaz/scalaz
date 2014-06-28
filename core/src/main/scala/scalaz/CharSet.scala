package scalaz

/** Newtype for strings representing character sets. */
@deprecated("CharSet is deprecated", "7.1")
final case class CharSet private(value: String)

@deprecated("CharSet is deprecated", "7.1")
object CharSet extends CharSetFunctions {

  def charset(v: String): CharSet = CharSet(v)

  val USASCII = charset("US-ASCII")

  val ISO8859 = charset("ISO-8859-1")

  val UTF8 = charset("UTF-8")

  val UTF16BE = charset("UTF-16BE")

  val UTF16LE = charset("UTF-16LE")

  val UTF16 = charset("UTF-16")
}

trait CharSetFunctions {
  @deprecated("CharSet is deprecated", "7.1")
  implicit def ToStringFromCharSet(charSet:CharSet): String = charSet.value
}
