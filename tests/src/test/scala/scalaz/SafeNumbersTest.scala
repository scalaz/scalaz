package scalaz

import scalaprops._
import Property.{implies, prop, property}

// testOnly *SafeNumbersTest*
object SafeNumbersTest extends Scalaprops {
  implicit def shrinker[A]: Shrink[A] = Shrink.empty[A]

  val validBigDecimal = property { i: java.math.BigDecimal =>
    prop(SafeNumbers.bigdecimal(i.toString, 2048) == Some(i))
  }

  val invalidBigDecimalEdgeCases =
    List(
      "N",
      "Inf",
      "-NaN",
      "+NaN",
      "e1",
      "1.1.1",
      "1 ",
      "NaN",
      "Infinity",
      "+Infinity",
      "-Infinity"
    ).map(s => prop(SafeNumbers.bigdecimal(s) == None)).reduce(_ and _)

  val validBigDecimalEdgeCases =
    List(
      ".0",
      "-.0",
      "0",
      "0.0",
      "-0.0", // zeroes
      "0000.1",
      "0.00001",
      "000.00001000" // various trailing zeros, should be preserved
    )
      .map { s =>
        (prop(
          SafeNumbers.bigdecimal(s).toString == Some(
            new java.math.BigDecimal(s)
          ).toString
        ))
      }
      .reduce(_ and _)

  val invalidBigDecimalText = property { s: String =>
    prop(SafeNumbers.bigdecimal(s) == None)
  }(Gen.alphaLowerString, Shrink.empty)

  val validBigIntegerEdgeCases =
    List(
      "00",
      "01",
      "0000001",
      "-9223372036854775807",
      "9223372036854775806",
      "-9223372036854775809",
      "9223372036854775808"
    )
      .map(
        s =>
          prop(SafeNumbers.biginteger(s) == Some(new java.math.BigInteger(s)))
      )
      .reduce(_ and _)

  val invalidBigIntegerEdgeCases =
    prop(
      List("0foo", "01foo", "0.1", "", "1 ")
        .map(SafeNumbers.biginteger(_))
        .forall(_.isEmpty)
    )

  val validBigInteger = property { i: java.math.BigInteger =>
    prop(SafeNumbers.biginteger(i.toString, 2048) == Some(i))
  }

  val invalidBigIntegerText = property { s: String =>
    prop(SafeNumbers.biginteger(s) == None)
  }(Gen.alphaLowerString, Shrink.empty)

  val validByte = property { i: Byte =>
    prop(SafeNumbers.byte(i.toString) == ByteSome(i))
  }

  val invalidByte = property { i: Long =>
    implies(
      i < Byte.MinValue || Byte.MaxValue < i,
      prop(SafeNumbers.byte(i.toString) == ByteNone)
    )
  }

  val invalidText = property { s: String =>
    prop(SafeNumbers.byte(s) == ByteNone)
  }(Gen.alphaLowerString, Shrink.empty)

  val validDouble = property { i: Double =>
    implies(!i.isNaN, prop(SafeNumbers.double(i.toString) == DoubleSome(i)))
  }

  val validDoubleFromInt = property { i: Int =>
    prop(SafeNumbers.double(i.toString) == DoubleSome(i.toDouble))
  }

  val validDoubleFromLong = property { i: Long =>
    prop(SafeNumbers.double(i.toString) == DoubleSome(i.toDouble))
  }

  val invalidDoubleEdgeCases =
    List("N", "Inf", "-NaN", "+NaN", "e1", "1.1.1", "1 ")
      .map(s => prop(SafeNumbers.double(s) == DoubleNone))
      .reduce(_ and _)

  val validDoubleEdgeCases =
    List(
      ".0",
      "-.0",
      "0",
      "0.0",
      "-0.0", // zeroes
      "0000.1",
      "0.00001",
      "000.00001000", // trailing zeros
      "NaN",
      "92233720368547758070", // overflows a Long significand
      "Infinity",
      "+Infinity",
      "-Infinity"
    )
      .map { s =>
        // better to do the comparison on strings to deal with NaNs
        (prop(
          SafeNumbers.double(s).toString == DoubleSome(s.toDouble).toString
        ))
      }
      .reduce(_ and _)

  val validMagicDouble =
    List("NaN", "Infinity", "+Infinity", "-Infinity")
      .map(SafeNumbers.double(_))
      .forall(!_.isEmpty)

  val invalidDoubleText = property { s: String =>
    prop(SafeNumbers.double(s) == DoubleNone)
  }(Gen.alphaLowerString, Shrink.empty)

  val validFloat = property { i: Float =>
    implies(!i.isNaN, prop(SafeNumbers.float(i.toString) == FloatSome(i)))
  }

  val validFloatFromInt = property { i: Int =>
    prop(SafeNumbers.float(i.toString) == FloatSome(i.toFloat))
  }

  val validFloatFromLong = property { i: Long =>
    prop(SafeNumbers.float(i.toString) == FloatSome(i.toFloat))
  }

  // note that in a stream, 1.1.1 may parse "1.1" leaving ".1"
  val invalidFloatEdgeCases =
    List("N", "Inf", "-NaN", "+NaN", "e1", "1.1.1")
      .map(s => prop(SafeNumbers.float(s) == FloatNone))
      .reduce(_ and _)

  val validFloatEdgeCases =
    List(
      ".0",
      "-.0",
      "0",
      "0.0",
      "-0.0", // zeroes
      "0000.1",
      "0.00001",
      "000.00001000", // trailing zeros
      "NaN",
      "92233720368547758070", // overflows a Long significand
      "Infinity",
      "+Infinity",
      "-Infinity"
    )
      .map { s =>
        // better to do the comparison on strings to deal with NaNs
        prop(SafeNumbers.float(s).toString == FloatSome(s.toFloat).toString)
      }
      .reduce(_ and _)

  val validFloatFromDouble = property { i: Double =>
    implies(
      !i.isNaN,
      prop(SafeNumbers.float(i.toString) == FloatSome(i.toFloat))
    )
  }

  val invalidFloatText = property { s: String =>
    prop(SafeNumbers.float(s) == FloatNone)
  }(Gen.alphaLowerString, Shrink.empty)

  val validInt = property { i: Int =>
    prop(SafeNumbers.int(i.toString) == IntSome(i))
  }

  val invalidInt = property { i: Long =>
    implies(
      i < Int.MinValue || Int.MaxValue < i,
      prop(SafeNumbers.int(i.toString) == IntNone)
    )
  }

  val invalidIntText = property { s: String =>
    prop(SafeNumbers.int(s) == IntNone)
  }(Gen.alphaLowerString, Shrink.empty)

  val validLongEdgeCases =
    List("00", "01", "0000001", "-9223372036854775807", "9223372036854775806")
      .map(s => prop(SafeNumbers.long(s) == LongSome(s.toLong)))
      .reduce(_ and _)

  val invalidLongEdgeCases =
    prop(
      List(
        "0foo",
        "01foo",
        "0.1",
        "",
        "1 ",
        "-9223372036854775809",
        "9223372036854775808"
      )
        .map(SafeNumbers.long)
        .forall(_.isEmpty)
    )

  val validLong = property { i: Long =>
    prop(SafeNumbers.long(i.toString) == LongSome(i))
  }

  val invalidLong = property { bi: BigInt =>
    val i = bi.underlying
    implies(i.bitLength > 63, prop(SafeNumbers.long(i.toString) == LongNone))
  }(Gen.genLargeBigInt, Shrink.empty)

  val invalidLongText = property { s: String =>
    prop(SafeNumbers.long(s) == LongNone)
  }(Gen.alphaLowerString, Shrink.empty)

  val validShort = property { i: Short =>
    prop(SafeNumbers.short(i.toString) == ShortSome(i))
  }

  val invalidShort = property { i: Long =>
    implies(
      i < Short.MinValue || Short.MaxValue < i,
      prop(SafeNumbers.short(i.toString) == ShortNone)
    )
  }

  val invalidShortText = property { s: String =>
    prop(SafeNumbers.short(s) == ShortNone)
  }(Gen.alphaLowerString, Shrink.empty)

}
