package scalaz

/** Newtype for strings representing character sets. */
sealed trait CharSet {
  val value: String
}

object CharSet extends CharSetFunctions {

  def charset(v: String): CharSet = new CharSet {
    val value = v
  }

  val USASCII = charset("US-ASCII")

  val ISO8859 = charset("ISO-8859-1")

  val UTF8 = charset("UTF-8")

  val UTF16BE = charset("UTF-16BE")

  val UTF16LE = charset("UTF-16LE")

  val UTF16 = charset("UTF-16")
}

trait CharSetFunctions {
  implicit def ToStringFromCharSet(charSet:CharSet): String = charSet.value
}
