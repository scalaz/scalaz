package fj.function;

import fj.F;
import fj.F2;
import static fj.Function.curry;
import static fj.pre.Semigroup.longAdditionSemigroup;
import static fj.pre.Semigroup.longMultiplicationSemigroup;

import static java.lang.Math.abs;

/**
 * Curried functions over Longs.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class Longs {
  private Longs() {
      throw new UnsupportedOperationException();
    }

  /**
   * Curried Long addition.
   */
  public static final F<Long, F<Long, Long>> add = longAdditionSemigroup.sum();

  /**
   * Curried Long multiplication.
   */
  public static final F<Long, F<Long, Long>> multiply = longMultiplicationSemigroup.sum();

  /**
   * Curried Long subtraction.
   */
  public static final F<Long, F<Long, Long>> subtract = curry(new F2<Long, Long, Long>() {
    public Long f(final Long x, final Long y) {
      return x - y;
    }
  });
  
  /**
   * Negation.
   */
  public static final F<Long, Long> negate = new F<Long, Long>() {
    public Long f(final Long x) {
      return x * -1L;
    }
  };

  /**
   * Absolute value.
   */
  public static final F<Long, Long> abs = new F<Long, Long>() {
    public Long f(final Long x) {
      return abs(x);
    }
  };

  /**
   * Remainder.
   */
  public static final F<Long, F<Long, Long>> remainder = curry(new F2<Long, Long, Long>() {
    public Long f(final Long a, final Long b) {
      return a % b;
    }
  });
}
