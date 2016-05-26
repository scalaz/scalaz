package scalaz
package std

trait StringInstances {
  implicit object stringInstance extends Monoid[String] with Show[String] with Equal[String] with Order[String] with IsEmpty[λ[α => String]] {
    type SA[α] = String
    def append(f1: String, f2: => String) = f1 + f2
    def zero: String = ""
    override def show(f: String) = '"' + f + '"'
    def order(x: String, y: String) = Ordering.fromInt(x.compareTo(y))
    override def equal(x: String, y: String) = x == y
    override def equalIsNatural: Boolean = true
    def empty[A] = zero
    def plus[A](f1: SA[A], f2: => SA[A]) = f1 + f2
    def isEmpty[A](s: SA[A]) = s == ""
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

  /**
   * Constructs a non-empty list with the value if it is not empty, otherwise, throws an error.
   */
  def charsNel(s:String) : Option[NonEmptyList[Char]] = list.toNel(s.toList)

  /**
   * Constructs a non-empty list with the given string if it is not empty, otherwise, returns the second argument.
   */
  def charsNel(s:String, e: => NonEmptyList[Char]) : NonEmptyList[Char] = charsNel(s) getOrElse e

  // Parsing functions.

  def parseBoolean(s:String): Validation[IllegalArgumentException, Boolean] =
    Validation.fromTryCatchThrowable[Boolean, IllegalArgumentException](s.toBoolean)

  def parseByte(s:String): Validation[NumberFormatException, Byte] =
    Validation.fromTryCatchThrowable[Byte, NumberFormatException](s.toByte)

  def parseShort(s:String): Validation[NumberFormatException, Short] =
    Validation.fromTryCatchThrowable[Short, NumberFormatException](s.toShort)

  def parseInt(s:String): Validation[NumberFormatException, Int] =
    Validation.fromTryCatchThrowable[Int, NumberFormatException](s.toInt)

  def parseLong(s:String): Validation[NumberFormatException, Long] =
    Validation.fromTryCatchThrowable[Long, NumberFormatException](s.toLong)

  def parseFloat(s:String): Validation[NumberFormatException, Float] =
    Validation.fromTryCatchThrowable[Float, NumberFormatException](s.toFloat)

  def parseDouble(s:String): Validation[NumberFormatException, Double] =
    Validation.fromTryCatchThrowable[Double, NumberFormatException](s.toDouble)

  def parseBigInt(s:String): Validation[NumberFormatException, BigInt] =
    Validation.fromTryCatchThrowable[BigInt, NumberFormatException](BigInt(s))

  def parseBigDecimal(s:String): Validation[NumberFormatException, BigDecimal] =
    Validation.fromTryCatchThrowable[BigDecimal, NumberFormatException](BigDecimal(s))
}

object string extends StringInstances with StringFunctions {
  object stringSyntax extends scalaz.syntax.std.ToStringOps
}
