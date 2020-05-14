package scalaz
package std

import _root_.java.math.{ BigInteger, BigDecimal }

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

import org.scalacheck._
import org.scalacheck.Prop.forAll

object StringTest extends SpecLite {
  import ArbitraryNumbers._

  checkAll(monoid.laws[String])
  checkAll(isEmpty.laws[λ[α => String]])

  checkAll(order.laws[String].withProp("benchmark", order.scalaOrdering[String]))

  "parseBoolean" in {
    import string.parseBoolean
    parseBoolean("true") must_===(Validation.success(true))
    parseBoolean("false") must_===(Validation.success(false))
    parseBoolean("1").isSuccess must_===(false)
  }

  "parseLong" in {
    import string.parseLong

    parseLong("abc") must_===(Validation.failure("abc does not represent a valid Long"))
    parseLong("-12345678901234567890") must_===(Validation.failure(s"-12345678901234567890 is outside of range for Long (${Long.MinValue} - ${Long.MaxValue})"))
  }

  "parseLong" ! forAll { (l: Long) =>
    string.parseLong(l.toString) must_=== Validation.success(l)
  }

  "parseInt" in {
    import string.parseInt

    parseInt("abc") must_===(Validation.failure("abc does not represent a valid Int"))
    parseInt("-123456789012") must_===(Validation.failure(s"-123456789012 is outside of range for Int (${Int.MinValue} - ${Int.MaxValue})"))
  }

  "parseInt" ! forAll { (i: Int) =>
    string.parseInt(i.toString) must_=== Validation.success(i)
  }

  "parseByte" in {
    import string.parseByte

    parseByte("abc") must_===(Validation.failure("abc does not represent a valid Byte"))
    parseByte("-1234") must_===(Validation.failure(s"-1234 is outside of range for Byte (${Byte.MinValue} - ${Byte.MaxValue})"))
  }

  "parseByte" ! forAll { (b: Byte) =>
    string.parseByte(b.toString) must_=== Validation.success(b)
  }

  "parseShort" in {
    import string.parseShort

    parseShort("abc") must_===(Validation.failure("abc does not represent a valid Short"))
    parseShort("-1234567") must_===(Validation.failure(s"-1234567 is outside of range for Short (${Short.MinValue} - ${Short.MaxValue})"))
  }

  "parseShort" ! forAll { (s: Short) =>
    string.parseShort(s.toString) must_=== Validation.success(s)
  }

  "parseDouble" in {
    import string.parseDouble

    parseDouble("abc") must_===(Validation.failure("abc does not represent a valid Double"))
    parseDouble("124567812345678234567812345678234567823456723456782345678234567823456782456789245678923456789245678923456789245678924567892345678923456928379487239847293874982374982739487239482398479238749823749827394872398472938479238749823794827394872398472938749823749827394872398472938479283749823749823749823794823794872394789") must_===(Validation.failure(s"124567812345678234567812345678234567823456723456782345678234567823456782456789245678923456789245678923456789245678924567892345678923456928379487239847293874982374982739487239482398479238749823749827394872398472938479238749823794827394872398472938749823749827394872398472938479283749823749823749823794823794872394789 is outside of range for Double"))
  }

  "parseDouble" ! forAll { (d: Double) =>
    string.parseDouble(d.toString) must_=== Validation.success(d)
  }

  "parseFloat" in {
    import string.parseFloat

    parseFloat("abc") must_===(Validation.failure("abc does not represent a valid Float"))
    parseFloat("124567812345678234567812345678234567823456723456782345678234567823456782456789245678923456789245678923456789245678924567892345678923456928379487239847293874982374982739487239482398479238749823749827394872398472938479238749823794827394872398472938749823749827394872398472938479283749823749823749823794823794872394789") must_===(Validation.failure(s"124567812345678234567812345678234567823456723456782345678234567823456782456789245678923456789245678923456789245678924567892345678923456928379487239847293874982374982739487239482398479238749823749827394872398472938479238749823794827394872398472938749823749827394872398472938479283749823749823749823794823794872394789 is outside of range for Float"))
  }

  "parseFloat" ! forAll { (f: Float) =>
    string.parseFloat(f.toString) must_=== Validation.success(f)
  }

  "parseBigInt" in {
    import string.parseBigInt

    parseBigInt("abc") must_===(Validation.failure("abc does not represent a valid BigInteger"))
  }

  "parseBigInt" ! forAll { (b: BigInteger) =>
    string.parseBigInt(b.toString) must_=== Validation.success(b)
  }

  "parseBigDecimal" in {
    import string.parseBigDecimal

    parseBigDecimal("abc") must_===(Validation.failure("abc does not represent a valid BigDecimal"))
  }

  "parseBigDecimal" ! forAll { (b: BigDecimal) =>
    string.parseBigDecimal(b.toString) must_=== Validation.success(b)
  }
}

object ArbitraryNumbers {
  import Gen.{ frequency, oneOf }

  private def arbitrary[T](implicit a: Arbitrary[T]): Gen[T] = a.arbitrary

  implicit val bigInteger: Arbitrary[BigInteger] = {
    val fromLong: Gen[BigInteger] = Gen.choose(Long.MinValue, Long.MaxValue).map(b => BigInteger.valueOf(b))
    val fromBigInt: Gen[BigInteger] = for {
      n <- arbitrary[BigInt]
    } yield {
      n.bigInteger
    }

    val fixed: Gen[BigInteger] = oneOf(
      BigInteger.ONE,
      BigInteger.TEN,
      BigInteger.ZERO
    )

    Arbitrary(frequency(
      (3, fixed),
      (20, fromLong),
      (20, fromBigInt)
    ))
  }

  implicit val bigDecimal: Arbitrary[BigDecimal] = {
    val fromDouble: Gen[BigDecimal] = Gen.choose(Double.MinValue, Double.MaxValue).map(d => new BigDecimal(d))
    val scaledBigInt: Gen[BigDecimal] = for {
      n <- arbitrary[BigInteger]
      s <- Gen.choose(-300, 300)
    } yield {
      new BigDecimal(n, s)
    }

    val fixed: Gen[BigDecimal] = oneOf(
      BigDecimal.ONE,
      BigDecimal.TEN,
      BigDecimal.ZERO
    )

    Arbitrary(frequency(
      (3, fixed),
      (20, fromDouble),
      (20, scaledBigInt)
    ))
  }
}
