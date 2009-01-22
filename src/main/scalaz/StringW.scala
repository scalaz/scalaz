// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

import validation.Validation

/**
 * Wraps <code>scala.Predef.String</code> and provides additional methods.
 *
 * @see scala.Predef.String
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait StringW {
  /**
   * The value of this string.
   */
  val s: String

  /**
   * Parses this string as a boolean and returns it in <code>Right</code>.
   */
  def parseBoolean: Validation[NumberFormatException, Boolean] = EitherW.throws(java.lang.Boolean.parseBoolean(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as a byte and returns it in <code>Right</code>.
   */
  def parseByte: Validation[NumberFormatException, Byte] = EitherW.throws(java.lang.Byte.parseByte(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as a double and returns it in <code>Right</code>.
   */
  def parseDouble: Validation[NumberFormatException, Double] = EitherW.throws(java.lang.Double.parseDouble(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as a float and returns it in <code>Right</code>.
   */
  def parseFloat: Validation[NumberFormatException, Float] = EitherW.throws(java.lang.Float.parseFloat(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as an int and returns it in <code>Right</code>.
   */
  def parseInt: Validation[NumberFormatException, Int] = EitherW.throws(java.lang.Integer.parseInt(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as a long and returns it in <code>Right</code>.
   */
  def parseLong: Validation[NumberFormatException, Long] = EitherW.throws(java.lang.Long.parseLong(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as a short and returns it in <code>Right</code>.
   */
  def parseShort: Validation[NumberFormatException, Short] = EitherW.throws(java.lang.Short.parseShort(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Returns the same String value if the given value is 1 otherwise pluralises this String by appending an "s" unless
   * this String ends with "y" and not one of ["ay", "ey", "iy", "oy", "uy"] in which case the 'y' character is chopped and "ies"
   * is appended.
   */
  def plural(n: Int) = if(n == 1) s else
                       if((s endsWith "y") && (List("ay", "ey","iy", "oy", "uy") forall (!s.endsWith(_)))) s.take(s.length - 1) + "ies"
                       else s + "s" 

  import xml._

  /**
   * Construct an XML node based on the given option value. If there is no value available, then an empty text node is returned,
   * otherwise, the string representation (using show) of the value is returned in an element with the given label.
   */
  def |:|[A](prefix: String, attributes: MetaData, scope: NamespaceBinding, a: Option[A])(implicit sh: Show[A]) =
    a match {
      case Some(t) => Elem(prefix, s, Null, TopScope, Text(Show.shows(t)))
      case None => Text("")
    }

  /**
   * Construct an XML node based on the given option value. If there is no value available, then an empty text node is returned,
   * otherwise, the string representation (using show) of the value is returned in an element with the given label.
   */
  def |:|[A](prefix: String, a: Option[A])(implicit sh: Show[A]): Node =
    |:|(prefix, Null, TopScope, a)

  /**
   * Construct an XML node based on the given option value. If there is no value available, then an empty text node is returned,
   * otherwise, the string representation (using show) of the value is returned in an element with the given label.
   */
  def |:|[A](a: Option[A])(implicit sh: Show[A]): Node =
    |:|(null, a)
}

/**
 * Functions over strings.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object StringW {
  /**
   * Wraps a <code>scala.Predef.String</code>.
   */
  implicit def StringWString(s: StringW) = s.s

  /**
   * Unwraps a <code>scala.Predef.String</code>.
   */
  implicit def StringStringW(ss: String): StringW = new StringW {
    val s = ss
  }

  /**
   * Wraps a <code>scala.List[Char]</code>.
   */
  implicit def StringWList(s: StringW) = s.s.toList

  /**
   * Unwraps a <code>scala.List[Char]</code>.
   */
  implicit def ListStringW(s: List[Char]) = StringStringW(s.mkString)
}
