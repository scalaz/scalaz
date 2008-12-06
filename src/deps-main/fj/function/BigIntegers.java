package fj.function;

import fj.F;
import fj.F2;
import fj.pre.Monoid;
import fj.data.List;
import static fj.Function.curry;

import java.math.BigInteger;

/**
 * Curried functions over Integers.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class BigIntegers {
  private BigIntegers() {
    throw new UnsupportedOperationException();
  }

  /**
   * Curried Integer addition.
   */
  public static final F<BigInteger, F<BigInteger, BigInteger>> add = curry(new F2<BigInteger, BigInteger, BigInteger>() {
    public BigInteger f(final BigInteger a1, final BigInteger a2) {
      return a1.add(a2);
    }
  });

  /**
   * Curried Integer multiplication.
   */
  public static final F<BigInteger, F<BigInteger, BigInteger>> multiply = curry(new F2<BigInteger, BigInteger, BigInteger>() {
    public BigInteger f(final BigInteger a1, final BigInteger a2) {
      return a1.multiply(a2);
    }
  });

  /**
   * Curried Integer subtraction.
   */
  public static final F<BigInteger, F<BigInteger, BigInteger>> subtract = curry(new F2<BigInteger, BigInteger, BigInteger>() {
    public BigInteger f(final BigInteger a1, final BigInteger a2) {
      return a1.subtract(a2);
    }
  });

  /**
   * Negation.
   */
  public static final F<BigInteger, BigInteger> negate = new F<BigInteger, BigInteger>() {
    public BigInteger f(final BigInteger i) {
      return i.negate();
    }
  };

  /**
   * Absolute value.
   */
  public static final F<BigInteger, BigInteger> abs = new F<BigInteger, BigInteger>() {
    public BigInteger f(final BigInteger i) {
      return i.abs();
    }
  };

  /**
   * Remainder.
   */
  public static final F<BigInteger, F<BigInteger, BigInteger>> remainder = curry(new F2<BigInteger, BigInteger, BigInteger>() {
    public BigInteger f(final BigInteger a1, final BigInteger a2) {
      return a1.remainder(a2);
    }
  });

  /**
   * Power.
   */
  public static final F<BigInteger, F<Integer, BigInteger>> power = curry(new F2<BigInteger, Integer, BigInteger>() {
    public BigInteger f(final BigInteger a1, final Integer a2) {
      return a1.pow(a2);
    }
  });

  /**
   * Sums a list of big integers.
   *
   * @param ints A list of big integers to sum.
   * @return The sum of the big integers in the list.
   */
  public static BigInteger sum(final List<BigInteger> ints) {
    return Monoid.bigintAdditionMonoid.sumLeft(ints);
  }

  /**
   * Returns the product of a list of big integers.
   *
   * @param ints A list of big integers to multiply together.
   * @return The product of the big integers in the list.
   */
  public static BigInteger product(final List<BigInteger> ints) {
    return Monoid.bigintMultiplicationMonoid.sumLeft(ints);
  }
}
