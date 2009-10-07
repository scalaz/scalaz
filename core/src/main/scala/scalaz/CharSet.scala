package scalaz

sealed trait CharSet {
  val value: String
}

object CharSet {
  def charSet(v: String) = new CharSet {
    val value = v
  }

  implicit def CharSetFrom(charSet:CharSet) = charSet.value

  val USASCII = charSet("US-ASCII")

  val ISO8859 = charSet("ISO-8859-1")

  val UTF8 = charSet("UTF-8")

  val UTF16BE = charSet("UTF-16BE")

  val UTF16LE = charSet("UTF-16LE")

  val UTF16 = charSet("UTF-16")
}
