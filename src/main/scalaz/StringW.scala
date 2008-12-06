// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

import validation.Validation

/**
 * Wraps <code>scala.Predef.String</code> and provides additional methods.
 *
 * @see scala.Predef.String
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
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
  val parseBoolean: Validation[NumberFormatException, Boolean] = EitherW.throws(java.lang.Boolean.parseBoolean(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as a byte and returns it in <code>Right</code>.
   */
  val parseByte: Validation[NumberFormatException, Byte] = EitherW.throws(java.lang.Byte.parseByte(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as a double and returns it in <code>Right</code>.
   */
  val parseDouble: Validation[NumberFormatException, Double] = EitherW.throws(java.lang.Double.parseDouble(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as a float and returns it in <code>Right</code>.
   */
  val parseFloat: Validation[NumberFormatException, Float] = EitherW.throws(java.lang.Float.parseFloat(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as an int and returns it in <code>Right</code>.
   */
  val parseInt: Validation[NumberFormatException, Int] = EitherW.throws(java.lang.Integer.parseInt(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as a long and returns it in <code>Right</code>.
   */
  val parseLong: Validation[NumberFormatException, Long] = EitherW.throws(java.lang.Long.parseLong(s)).left.map(_.asInstanceOf[NumberFormatException])

  /**
   * Parses this string as a short and returns it in <code>Right</code>.
   */
  val parseShort: Validation[NumberFormatException, Short] = EitherW.throws(java.lang.Short.parseShort(s)).left.map(_.asInstanceOf[NumberFormatException])
}

/**
 * Functions over strings.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
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
