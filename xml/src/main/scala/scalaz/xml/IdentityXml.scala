package scalaz
package xml

/**
 *  A wrapper for a value of any type on which XML-like operations (e.g. parsing) may be performed.
 */
sealed trait IdentityXml[A] {
  val value: A

  import xml.pp.{XPrint, Config}

  /**
   * Produces the XML pretty-printed output of this value using the given configuration.
   */
  def sxprint(c: Config)(implicit x: XPrint[A]): List[Char] =
    x.xprint(value)(c)

  /** Produces the XML pretty-printed output of this value using the given configuration. */
  def sxprints(c: Config)(implicit x: XPrint[A]): String =
    x.xprints(value)(c)

  /**
   * Prints the XML pretty-printed output of this value to the standard output stream using the given configuration.
   */
  def xprint(c: Config)(implicit x: XPrint[A]): Unit =
    Predef.print(sxprints(c))

  /**
   * Prints the XML pretty-printed output of this value to the standard output stream using the given configuration followed by a newline.
   */
  def xprintln(c: Config)(implicit x: XPrint[A]): Unit =
    Predef.println(sxprints(c))

  /**
   * Prints the XML pretty-printed output of this value to the standard output stream using the default configuration.
   */
  def xdprint(implicit x: XPrint[A]): Unit =
    xprint(Config.config())

  /**
   * Prints the XML pretty-printed output of this value to the standard output stream using the default configuration followed by a newline.
   */
  def xdprintln(implicit x: XPrint[A]): Unit =
    xprintln(Config.config())

  /**
   * Writes the XML pretty-printed output of this value to the given file using the given configuration.
   */
  def xwriteFile(f: String)(c: Config)(implicit x: XPrint[A]): Unit = {
    val o = new java.io.FileWriter(f)
    try {
      o.write(sxprints(c))
    } finally {
      o.close
    }
  }

  /**
   * Appends the XML pretty-printed output of this value to the given file using the given configuration.
   */
  def xappendFile(f: String)(c: Config)(implicit x: XPrint[A]): Unit = {
    val o = new java.io.FileWriter(f, true)
    try {
      o.write(sxprints(c))
    } finally {
      o.close
    }
  }

  import xml._
  import CData._
  import Token._

  /**
   * Constructs an XML attribute using this string value as the key and the given argument as the value.
   */
  def -=-(v: String)(implicit ev: A =:= String): Attr =
    Attr.attrs(value, v)

  /**
   * Produces a string with an associated line-number from this XML data source.
   */
  def linenumber(n: Line)(implicit x: XSource[A]): LStr =
    x.linenumber(value, n)

  /**
   * Produces a list of tokens from this XML data source with the line-number 1.
   */
  def tokens(implicit x: XSource[A]): List[Token] =
    x.tokens(value)

  /**
   * Parse this XML data source into a list of content values.
   */
  def parseXml(implicit x: XSource[A]): List[Content] =
    x.parseXml(value)

  /**
   * Parse this XML data source into a potential element value.
   */
  def parseXmlDoc(implicit x: XSource[A]): Option[Element] =
    x.parseXmlDoc(value)

  /**
   * Parse this filename as a XML data source into a list of content values.
   */
  def parseXmlFile(implicit ev: A =:= String): List[Content] = {
    val i = new java.io.FileReader(value)
    try {
      val b = new collection.mutable.ListBuffer[Char]
      var c = i.read

      while(c != -1) {
        b += c.toChar
        c = i.read
      }

      val w = b.toList
      implicitly[XSource[List[Char]]].parseXml(w)
    } finally {
      i.close
    }
  }

  /**
   * Parse this filename as a XML data source into a potential element value.
   */
  def parseXmlDocFile(implicit ev: A =:= String): Option[Element] = {
    val i = new java.io.FileReader(value)
    try {
      val b = new collection.mutable.ListBuffer[Char]
      var c = i.read

      while(c != -1) {
        b += c.toChar
        c = i.read
      }

      val w = b.toList
      implicitly[XSource[List[Char]]].parseXmlDoc(w)
    } finally {
      i.close
    }
  }
}

trait IdentityXmls {
  implicit def ToIdentityXml[A](a: A): IdentityXml[A] =
    new IdentityXml[A] {
      val value = a
    }
}

object IdentityXml extends IdentityXmls
