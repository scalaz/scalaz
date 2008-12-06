package fj.function;

import fj.F;
import fj.F2;
import fj.data.List;
import static fj.Function.curry;
import static fj.pre.Semigroup.intAdditionSemigroup;
import static fj.pre.Semigroup.intMultiplicationSemigroup;
import fj.pre.Monoid;

import static java.lang.Math.abs;

/**
 * Curried functions over Integers.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class Integers {
  private Integers() {
    throw new UnsupportedOperationException();
  }

  /**
   * Curried Integer addition.
   */
  public static final F<Integer, F<Integer, Integer>> add = intAdditionSemigroup.sum();

  /**
   * Curried Integer multiplication.
   */
  public static final F<Integer, F<Integer, Integer>> multiply = intMultiplicationSemigroup.sum();

  /**
   * Curried Integer subtraction.
   */
  public static final F<Integer, F<Integer, Integer>> subtract = curry(new F2<Integer, Integer, Integer>() {
    public Integer f(final Integer x, final Integer y) {
      return x - y;
    }
  });

  /**
   * Negation.
   */
  public static final F<Integer, Integer> negate = new F<Integer, Integer>() {
    public Integer f(final Integer x) {
      return x * -1;
    }
  };

  /**
   * Absolute value.
   */
  public static final F<Integer, Integer> abs = new F<Integer, Integer>() {
    public Integer f(final Integer x) {
      return abs(x);
    }
  };

  /**
   * Remainder.
   */
  public static final F<Integer, F<Integer, Integer>> remainder = curry(new F2<Integer, Integer, Integer>() {
    public Integer f(final Integer a, final Integer b) {
      return a % b;
    }
  });

  /**
   * Power.
   */
  public static final F<Integer, F<Integer, Integer>> power = curry(new F2<Integer, Integer, Integer>() {
    public Integer f(final Integer a, final Integer b) {
      return (int) StrictMath.pow(a, b);
    }
  });

  /**
   * Evenness.
   */
  public static final F<Integer, Boolean> even = new F<Integer, Boolean>() {
    public Boolean f(final Integer i) {
      return i % 2 == 0;
    }
  };

  /**
   * Sums a list of integers.
   *
   * @param ints A list of integers to sum.
   * @return The sum of the integers in the list.
   */
  public static int sum(final List<Integer> ints) {
    return Monoid.intAdditionMonoid.sumLeft(ints);
  }

  /**
   * Returns the product of a list of integers.
   *
   * @param ints A list of integers to multiply together.
   * @return The product of the integers in the list.
   */
  public static int product(final List<Integer> ints) {
    return Monoid.intMultiplicationMonoid.sumLeft(ints);
  }

}
