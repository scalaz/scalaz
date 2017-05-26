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

  @deprecated("Unsafe. Use `charsNel` instead", "7.2")
  def charsNelErr(s:String, message: => String): NonEmptyList[Char] = charsNel(s, sys.error(message))

  @deprecated("Unsafe. Use `charsNel` instead", "7.2")
  def unsafeCharsNel(s:String) : NonEmptyList[Char] = charsNelErr(s, "cannot turn empty string into NonEmptyList")

  // Parsing functions.

  def parseBoolean(s:String): Validation[IllegalArgumentException, Boolean] =
    \/.fromTryCatchThrowable[Boolean, IllegalArgumentException](s.toBoolean).validation

  def parseByte(s:String): Validation[NumberFormatException, Byte] =
    \/.fromTryCatchThrowable[Byte, NumberFormatException](s.toByte).validation

  def parseShort(s:String): Validation[NumberFormatException, Short] =
    \/.fromTryCatchThrowable[Short, NumberFormatException](s.toShort).validation

  def parseInt(s:String): Validation[NumberFormatException, Int] =
    \/.fromTryCatchThrowable[Int, NumberFormatException](s.toInt).validation

  def parseLong(s:String): Validation[NumberFormatException, Long] =
    \/.fromTryCatchThrowable[Long, NumberFormatException](s.toLong).validation

  def parseFloat(s:String): Validation[NumberFormatException, Float] =
    \/.fromTryCatchThrowable[Float, NumberFormatException](s.toFloat).validation

  def parseDouble(s:String): Validation[NumberFormatException, Double] =
    \/.fromTryCatchThrowable[Double, NumberFormatException](s.toDouble).validation

  def parseBigInt(s:String): Validation[NumberFormatException, BigInt] =
    \/.fromTryCatchThrowable[BigInt, NumberFormatException](BigInt(s)).validation

  def parseBigDecimal(s:String): Validation[NumberFormatException, BigDecimal] =
    \/.fromTryCatchThrowable[BigDecimal, NumberFormatException](BigDecimal(s)).validation
}

object string extends StringInstances with StringFunctions {
  object stringSyntax extends scalaz.syntax.std.ToStringOps

  def parseBigInt(s:String): Validation[NumberFormatException, BigInt] = try {
    Success(BigInt(s))
  } catch {
    case e: NumberFormatException => Failure(e)
  }

  def parseBigDecimal(s:String): Validation[NumberFormatException, BigDecimal] = try {
    Success(BigDecimal(s))
  } catch {
    case e: NumberFormatException => Failure(e)
  }
}
