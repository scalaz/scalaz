package scalaz
package std

trait StringInstances {
  implicit object stringInstance extends Monoid[String] with Show[String] with Equal[String] with Order[String]{
    def append(f1: String, f2: => String): String = f1 + f2
    def zero: String = ""
    def show(f: String): List[Char] = f.toList
    def order(x: String, y: String): Ordering = Ordering.fromInt(x.compareTo(y))
    override def equal(x: String, y: String): Boolean = x == y
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