package scalaz
package std

trait StringInstances {
  implicit object stringInstance extends Monoid[String] with Show[String] with Equal[String] with Order[String]{
    def append(f1: String, f2: => String): String = f1 + f2
    def zero: String = ""
    def show(f: String): List[Char] = f.toList
    def order(x: String, y: String): Ordering = Ordering.fromInt(x.compareTo(y))
  }
}

trait StringFunctions {
  /**
   * Returns the same String value if the given value is 1 otherwise pluralises this String by appending an "s" unless
   * this String ends with "y" and not one of ["ay", "ey", "iy", "oy", "uy"] in which case the 'y' character is chopped and "ies"
   * is appended.
   */
  def plural(s:String, n: Long): String = if(n == 1L) s else
  if((s endsWith "y") && (List("ay", "ey","iy", "oy", "uy") forall (!s.endsWith(_)))) s.take(s.length - 1) + "ies"
  else s + "s"

  import xml._

  /**
   * Construct an XML node based on the given option value. If there is no value available, then an empty text node is returned,
   * otherwise, the string representation (using show) of the value is returned in an element with the given label.
   */
  def node[A: Show](s:String, prefix: String, attributes: MetaData, scope: NamespaceBinding, a: Option[A]): Node =
    a match {
      case Some(t) => Elem(prefix, s, Null, TopScope, Show[A].text(t))
      case None => Text("")
    }

  /**
   * Construct an XML node based on the given option value. If there is no value available, then an empty text node is returned,
   * otherwise, the string representation (using show) of the value is returned in an element with the given label.
   */
  def node[A: Show](s:String, prefix: String, a: Option[A]): Node =
    node(s, prefix, Null, TopScope, a)

  /**
   * Construct an XML node based on the given option value. If there is no value available, then an empty text node is returned,
   * otherwise, the string representation (using show) of the value is returned in an element with the given label.
   */
  def node[A: Show](s:String, a: Option[A]): Node =
    node(s, null, a)

  def encode(s:String)(implicit c: CharSet): Array[Byte] = s getBytes c.value

  /**
   * Constructs a non-empty list with the value if it is not empty, otherwise, throws an error.
   */
  def charsNel(s:String) : Option[NonEmptyList[Char]] = list.toNel(s.toList)

  /**
   * Constructs a non-empty list with the given string if it is not empty, otherwise, returns the second argument.
   */
  def charsNel(s:String, e: => NonEmptyList[Char]) : NonEmptyList[Char] = charsNel(s) getOrElse e

  def charsNelErr(s:String, message: => String): NonEmptyList[Char] = charsNel(s, sys.error(message))

  def unsafeCharsNel(s:String) : NonEmptyList[Char] = charsNelErr(s, "cannot turn empty string into NonEmptyList")

//  import _root_.java.io.FileInputStream
//
//  def readFile[X](x: X, f: (X, Byte) => X): X = {
//    val in = new FileInputStream(s)
//
//    try {
//      in.elements.foldLeft(x)(f)
//    } finally {
//      in.close
//    }
//  }
//
//  def fileEach(f: Byte => Unit) {
//    val in = new FileInputStream(s)
//
//    try {
//      in.elements.foreach(f)
//    } finally {
//      in.close
//    }
//  }
//
//  def readLines[X, Y](x: X, f: (X, Char) => X, y: Y, g: (Y, X) => Y) {
//    val in = new FileInputStream(s)
//
//    try {
//      var t = x
//      var u = y
//      val i = in.elements
//      val lineSeparators = List('\r'.toByte, '\n'.toByte)
//
//      while(i.hasNext) {
//        val c = i.next.toChar
//
//        if(lineSeparators.contains(c)) {
//          u = g(u, t)
//          t = x
//        } else
//          t = f(t, c)
//      }
//
//      u
//    } finally {
//      in.close
//    }
//  }

  // Parsing functions.
  import Validation.{success, failure}

  def parseBoolean(s:String): Validation[NumberFormatException, Boolean] = try {
    success(s.toBoolean)
  } catch {
    case e: NumberFormatException => failure(e)
  }

  def parseByte(s:String): Validation[NumberFormatException, Byte] = try {
    success(s.toByte)
  } catch {
    case e: NumberFormatException => failure(e)
  }

  def parseShort(s:String): Validation[NumberFormatException, Short] = try {
    success(s.toShort)
  } catch {
    case e: NumberFormatException => failure(e)
  }

  def parseInt(s:String): Validation[NumberFormatException, Int] = try {
    success(s.toInt)
  } catch {
    case e: NumberFormatException => failure(e)
  }

  def parseLong(s:String): Validation[NumberFormatException, Long] = try {
    success(s.toLong)
  } catch {
    case e: NumberFormatException => failure(e)
  }

  def parseFloat(s:String): Validation[NumberFormatException, Float] = try {
    success(s.toFloat)
  } catch {
    case e: NumberFormatException => failure(e)
  }

  def parseDouble(s:String): Validation[NumberFormatException, Double] = try {
    success(s.toDouble)
  } catch {
    case e: NumberFormatException => failure(e)
  }
}

object string extends StringInstances with StringFunctions {
  object stringSyntax extends scalaz.syntax.std.ToStringV
}