package scalaz
package wrap

sealed trait StringW {
  val s: String

  import Validation._
  import InputStreamW._
  import xml._

  /**
   * Returns the same String value if the given value is 1 otherwise pluralises this String by appending an "s" unless
   * this String ends with "y" and not one of ["ay", "ey", "iy", "oy", "uy"] in which case the 'y' character is chopped and "ies"
   * is appended.
   */
  def plural(n: Long): String = if (n == 1L) s
  else
  if ((s endsWith "y") && (List("ay", "ey", "iy", "oy", "uy") forall (!s.endsWith(_)))) s.take(s.length - 1) + "ies"
  else s + "s"

  /**
   * Construct an XML node based on the given option value. If there is no value available, then an empty text node is returned,
   * otherwise, the string representation (using show) of the value is returned in an element with the given label.
   */
  def node[A: Show](
                       a: Option[A],
                       prefix: Option[String] = None,
                       attributes: MetaData = Null,
                       scope: NamespaceBinding = TopScope
                       ): Node =
    a match {
      case Some(t) => Elem(prefix.orNull, s, attributes, scope, Text(implicitly[Show[A]].shows(t)))
      case None => Text("")
    }

  /**
   * Constructs a non-empty list with the value if it is not empty, otherwise, throws an error.
   */
  def charsNel: Option[NonEmptyList[Char]] =
    s.toList match {
      case Nil => None
      case h :: t => Some(NonEmptyList.nel(h, t))
    }

  /**
   * Constructs a non-empty list with the given string if it is not empty, otherwise, returns the second argument.
   */
  def charsNel(e: => NonEmptyList[Char]): NonEmptyList[Char] =
    charsNel getOrElse e

  import java.io.FileInputStream

  def readFile[X](x: X, f: (X, Byte) => X): X = {
    val in = new FileInputStream(s)

    try {
      in.elements.foldLeft(x)(f)
    } finally {
      in.close
    }
  }

  def fileEach(f: Byte => Unit) {
    val in = new FileInputStream(s)

    try {
      in.elements.foreach(f)
    } finally {
      in.close
    }
  }

  def readLines[X, Y](x: X, f: (X, Char) => X, y: Y, g: (Y, X) => Y) {
    val in = new FileInputStream(s)

    try {
      var t = x
      var u = y
      val i = in.elements
      val lineSeparators = List('\r'.toByte, '\n'.toByte)

      while (i.hasNext) {
        val c = i.next.toChar

        if (lineSeparators.contains(c)) {
          u = g(u, t)
          t = x
        } else
          t = f(t, c)
      }

      u
    } finally {
      in.close
    }
  }

  // Parsing functions.

  def parseBoolean: Validation[NumberFormatException, Boolean] = try {
    success(s.toBoolean)
  } catch {
    case e: NumberFormatException => failure(e)
  }

  def parseByte: Validation[NumberFormatException, Byte] = try {
    success(java.lang.Byte.parseByte(s))
  } catch {
    case e: NumberFormatException => failure(e)
  }

  def parseShort: Validation[NumberFormatException, Short] = try {
    success(java.lang.Short.parseShort(s))
  } catch {
    case e: NumberFormatException => failure(e)
  }

  def parseInt: Validation[NumberFormatException, Int] = try {
    success(java.lang.Integer.parseInt(s))
  } catch {
    case e: NumberFormatException => failure(e)
  }

  def parseLong: Validation[NumberFormatException, Long] = try {
    success(java.lang.Long.parseLong(s))
  } catch {
    case e: NumberFormatException => failure(e)
  }

  def parseFloat: Validation[NumberFormatException, Float] = try {
    success(java.lang.Float.parseFloat(s))
  } catch {
    case e: NumberFormatException => failure(e)
  }

  def parseDouble: Validation[NumberFormatException, Double] = try {
    success(java.lang.Double.parseDouble(s))
  } catch {
    case e: NumberFormatException => failure(e)
  }
}

object StringW extends StringWs

trait StringWs {
  implicit def StringTo(ss: String): StringW = new StringW {
    val s = ss
  }
}
