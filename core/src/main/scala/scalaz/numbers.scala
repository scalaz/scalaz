package scalaz

import java.util.regex.Pattern
import scala.annotation._
import scala.util.control.NoStackTrace

/**
  * Total, fast, number parsing.
  *
  * The Java and Scala standard libraries throw exceptions when we attempt to
  * parse an invalid number. Unfortunately, exceptions are very expensive, and
  * untrusted data can be maliciously constructed to DOS a server.
  *
  * This suite of functions mitigates against such attacks by building up the
  * numbers one character at a time, which has been shown through extensive
  * benchmarking to be orders of magnitude faster than exception-throwing stdlib
  * parsers, for valid and invalid inputs. This approach, proposed by alexknvl,
  * was also benchmarked against regexp-based pre-validation.
  *
  * Note that although the behaviour is identical to the Java stdlib when given
  * the canonical form of a primitive (i.e. the .toString) of a number there may
  * be differences in behaviour for non-canonical forms. e.g. the Java stdlib
  * may reject "1.0" when parsed as an `BigInteger` but we may parse it as a
  * `1`, although "1.1" would be rejected. Parsing of `BigDecimal` preserves the
  * trailing zeros on the right but not on the left, e.g. "000.00001000" will be
  * "1.000e-5", which is useful in cases where the trailing zeros denote
  * measurement accuracy.
  *
  * `BigInteger`, `BigDecimal`, `Float` and `Double` have a configurable bit
  * limit on the size of the significand, to avoid OOM style attacks, which is
  * 128 bits by default.
  *
  * Results are contained in a specialisation of Option that avoids boxing.
  */
// TODO hex radix
// TODO octal radix
private[scalaz] object SafeNumbers {
  import UnsafeNumbers.UnsafeNumber

  def byte(num: String): ByteOption =
    try ByteSome(UnsafeNumbers.byte(num))
    catch { case UnsafeNumber => ByteNone }

  def short(num: String): ShortOption =
    try ShortSome(UnsafeNumbers.short(num))
    catch { case UnsafeNumber => ShortNone }

  def int(num: String): IntOption =
    try IntSome(UnsafeNumbers.int(num))
    catch { case UnsafeNumber => IntNone }

  def long(num: String): LongOption =
    try LongSome(UnsafeNumbers.long(num))
    catch { case UnsafeNumber => LongNone }

  def biginteger(
    num: String,
    max_bits: Int = 128
  ): Option[java.math.BigInteger] =
    try Some(UnsafeNumbers.biginteger(num, max_bits))
    catch { case UnsafeNumber => None }

  def float(num: String, max_bits: Int = 128): FloatOption =
    try FloatSome(UnsafeNumbers.float(num, max_bits))
    catch { case UnsafeNumber => FloatNone }

  def double(num: String, max_bits: Int = 128): DoubleOption =
    try DoubleSome(UnsafeNumbers.double(num, max_bits))
    catch { case UnsafeNumber => DoubleNone }

  def bigdecimal(
    num: String,
    max_bits: Int = 128
  ): Option[java.math.BigDecimal] =
    try Some(UnsafeNumbers.bigdecimal(num, max_bits))
    catch { case UnsafeNumber => None }

}

// specialised Options to avoid boxing. Prefer .isEmpty guarded access to .value
// for higher performance: pattern matching is slightly slower.

private[scalaz] sealed abstract class ByteOption {
  def isEmpty: Boolean
  def value: Byte
}
private[scalaz] case object ByteNone extends ByteOption {
  def isEmpty = true
  def value = ???
}
private[scalaz] case class ByteSome(value: Byte) extends ByteOption {
  def isEmpty = false
}

private[scalaz] sealed abstract class ShortOption {
  def isEmpty: Boolean
  def value: Short
}
private[scalaz] case object ShortNone extends ShortOption {
  def isEmpty = true
  def value = ???
}
private[scalaz] case class ShortSome(value: Short) extends ShortOption {
  def isEmpty = false
}

private[scalaz] sealed abstract class IntOption {
  def isEmpty: Boolean
  def value: Int
}
private[scalaz] case object IntNone extends IntOption {
  def isEmpty = true
  def value = ???
}
private[scalaz] case class IntSome(value: Int) extends IntOption {
  def isEmpty = false
}

private[scalaz] sealed abstract class LongOption {
  def isEmpty: Boolean
  def value: Long
}
private[scalaz] case object LongNone extends LongOption {
  def isEmpty = true
  def value = ???
}
private[scalaz] case class LongSome(value: Long) extends LongOption {
  def isEmpty = false
}

private[scalaz] sealed abstract class FloatOption {
  def isEmpty: Boolean
  def value: Float
}
private[scalaz] case object FloatNone extends FloatOption {
  def isEmpty = true
  def value = ???
}
private[scalaz] case class FloatSome(value: Float) extends FloatOption {
  def isEmpty = false
}

private[scalaz] sealed abstract class DoubleOption {
  def isEmpty: Boolean
  def value: Double
}
private[scalaz] case object DoubleNone extends DoubleOption {
  def isEmpty = true
  def value = ???
}
private[scalaz] case class DoubleSome(value: Double) extends DoubleOption {
  def isEmpty = false
}

// The underlying implementation uses an exception that has no stack trace for
// the failure private[scalaz] case, which is 20x faster than retaining stack traces. Therefore,
// we require no boxing of the results on the happy path. This slows down the
// unhappy path a little bit, but it's still on the same order of magnitude as
// the happy path.
//
// This API should only be used by people who know what they are doing. Note
// that Reader implementations consume one character beyond the number that is
// parsed, because there is no terminator character.
private[scalaz] object UnsafeNumbers {

  // should never escape into user code
  case object UnsafeNumber
      extends Exception(
        "if you see this a dev made a mistake using UnsafeNumbers"
      )
      with NoStackTrace

  def byte(num: String): Byte =
    byte_(new FastStringReader(num), true)
  def byte_(in: java.io.Reader, consume: Boolean): Byte =
    long__(in, Byte.MinValue, Byte.MaxValue, consume).toByte

  def short(num: String): Short =
    short_(new FastStringReader(num), true)
  def short_(in: java.io.Reader, consume: Boolean): Short =
    long__(in, Short.MinValue, Short.MaxValue, consume).toShort

  def int(num: String): Int =
    int_(new FastStringReader(num), true)
  def int_(in: java.io.Reader, consume: Boolean): Int =
    long__(in, Int.MinValue, Int.MaxValue, consume).toInt

  def long(num: String): Long =
    long_(new FastStringReader(num), true)
  def long_(in: java.io.Reader, consume: Boolean): Long =
    long__(in, Long.MinValue, Long.MaxValue, consume)

  def biginteger(num: String, max_bits: Int): java.math.BigInteger =
    biginteger_(new FastStringReader(num), true, max_bits)
  def biginteger_(
    in: java.io.Reader,
    consume: Boolean,
    max_bits: Int
  ): java.math.BigInteger = {
    var current: Int = in.read()
    var negative = false

    if (current == '-') {
      negative = true
      current = in.read()
    } else if (current == '+')
      current = in.read()
    if (current == -1) throw UnsafeNumber

    bigdecimal__(in, consume, negative, current, true, max_bits).unscaledValue
  }

  // measured faster than Character.isDigit
  @inline private[this] def isDigit(i: Int): Boolean =
    '0' <= i && i <= '9'

  // is it worth keeping this custom long__ instead of using biginteger since it
  // is approximately double the performance.
  def long__(in: java.io.Reader, lower: Long, upper: Long, consume: Boolean): Long = {
    var current: Int = 0

    current = in.read()
    if (current == -1) throw UnsafeNumber
    var negative = false
    if (current == '-') {
      negative = true
      current = in.read()
      if (current == -1) throw UnsafeNumber
    } else if (current == '+') {
      current = in.read()
      if (current == -1) throw UnsafeNumber
    }

    if (!isDigit(current))
      throw UnsafeNumber

    var accum: Long = 0L
    do {
      val c = current - '0'
      if (accum <= longunderflow)
        if (accum < longunderflow)
          throw UnsafeNumber
        else if (accum == longunderflow && c == 9)
          throw UnsafeNumber
      // count down, not up, because it is larger
      accum = accum * 10 - c // should never underflow
      current = in.read()
    } while (current != -1 && isDigit(current))

    if (consume && current != -1) throw UnsafeNumber

    if (negative)
      if (accum < lower || upper < accum) throw UnsafeNumber
      else accum
    else if (accum == Long.MinValue)
      throw UnsafeNumber
    else {
      accum = -accum
      if (accum < lower || upper < accum) throw UnsafeNumber
      else accum
    }
  }

  def float(num: String, max_bits: Int): Float =
    float_(new FastStringReader(num), true, max_bits)
  def float_(in: java.io.Reader, consume: Boolean, max_bits: Int): Float =
    double_(in, consume, max_bits).toFloat

  def double(num: String, max_bits: Int): Double =
    double_(new FastStringReader(num), true, max_bits)
  def double_(in: java.io.Reader, consume: Boolean, max_bits: Int): Double = {
    var current: Int = in.read()
    var negative = false

    def readall(s: String): Unit = {
      var i = 0
      while (i < s.length) {
        current = in.read()
        if (current != s(i)) throw UnsafeNumber
        i += 1
      }
      current = in.read() // to be consistent read the terminator
      if (consume && current != -1) throw UnsafeNumber
    }

    if (current == 'N') {
      readall("aN")
      return Double.NaN
    }

    if (current == '-') {
      negative = true
      current = in.read()
    } else if (current == '+')
      current = in.read()

    if (current == 'I') {
      readall("nfinity")
      if (negative) return Double.NegativeInfinity
      else return Double.PositiveInfinity
    }

    if (current == -1) throw UnsafeNumber

    val res = bigdecimal__(in, consume, negative, current, false, max_bits)
    // BigDecimal doesn't have a negative zero, so we need to apply manually
    if (negative && res.unscaledValue == java.math.BigInteger.ZERO) -0.0
    else res.doubleValue
  }

  def bigdecimal(num: String, max_bits: Int): java.math.BigDecimal =
    bigdecimal_(new FastStringReader(num), true, max_bits)
  def bigdecimal_(
    in: java.io.Reader,
    consume: Boolean,
    max_bits: Int
  ): java.math.BigDecimal = {
    var current: Int = in.read()
    var negative = false

    if (current == '-') {
      negative = true
      current = in.read()
    } else if (current == '+')
      current = in.read()
    if (current == -1) throw UnsafeNumber

    bigdecimal__(in, consume, negative, current, false, max_bits)
  }

  def bigdecimal__(
    in: java.io.Reader,
    consume: Boolean,
    negative: Boolean,
    initial: Int,
    int_only: Boolean,
    max_bits: Int
  ): java.math.BigDecimal = {
    var current: Int = initial
    // record the significand as Long until it overflows, then swap to BigInteger
    var sig: Long = -1 // -1 means it hasn't been seen yet
    var sig_ : java.math.BigInteger = null // non-null wins over sig
    var dot: Int = 0 // counts from the right
    var exp: Int = 0 // implied

    def read(): Int = {
      current = in.read()
      if (current == -1) throw UnsafeNumber
      current
    }

    def advance(): Boolean = {
      current = in.read()
      current != -1
    }

    // skip trailing zero on the left
    while (current == '0') {
      sig = 0
      if (!advance())
        return java.math.BigDecimal.ZERO
    }

    def push_sig(): Unit = {
      val c = current - '0'
      // would be nice if there was a fused instruction...
      if (sig_ != null) {
        sig_ = sig_
          .multiply(java.math.BigInteger.TEN)
          .add(bigintegers(c))
        // arbitrary limit on BigInteger size to avoid OOM attacks
        if (sig_.bitLength >= max_bits)
          throw UnsafeNumber
      } else if (sig >= longoverflow)
        sig_ = java.math.BigInteger
          .valueOf(sig)
          .multiply(java.math.BigInteger.TEN)
          .add(bigintegers(c))
      else if (sig < 0) sig = c
      else sig = sig * 10 + c
    }

    def significand() =
      if (sig <= 0) java.math.BigDecimal.ZERO
      else {
        val res =
          if (sig_ != null)
            new java.math.BigDecimal(sig_)
          else
            new java.math.BigDecimal(sig)
        if (negative) res.negate else res
      }

    while (isDigit(current)) {
      push_sig()
      if (!advance())
        return significand()
    }

    if (int_only) {
      if (consume && current != -1)
        throw UnsafeNumber
      return significand()
    }

    if (current == '.') {
      if (sig < 0) sig = 0 // e.g. ".1" is shorthand for "0.1"
      if (!advance())
        return significand()
      while (isDigit(current)) {
        dot += 1
        if (sig > 0 || current != '0')
          push_sig()
        // overflowed...
        if (dot < 0) throw UnsafeNumber
        advance()
      }
    }

    if (sig < 0) throw UnsafeNumber // no significand

    if (current == 'E' || current == 'e')
      exp = int_(in, consume)
    else if (consume && current != -1)
      throw UnsafeNumber

    val scale = if (dot < 1) exp else exp - dot
    val res = significand()
    if (scale != 0)
      res.scaleByPowerOfTen(scale)
    else
      res
  }
  // note that bigdecimal does not have a negative zero
  val bigintegers: Array[java.math.BigInteger] =
    (0L to 9L).map(java.math.BigInteger.valueOf(_)).toArray
  val longunderflow: Long = Long.MinValue / 10L
  val longoverflow: Long = Long.MaxValue / 10L

}

// java.io.StringReader uses a lock, which reduces perf by x2
private[scalaz] final class FastStringReader(s: String) extends java.io.Reader {
  // we only ever read one char at a time
  def read(cbuf: Array[Char], off: Int, len: Int): Int = ???

  private var i: Int = 0
  def close(): Unit = ()
  override def read(): Int =
    if (i == s.length()) -1
    else {
      val res = s.codePointAt(i)
      i += 1
      res
    }
}
