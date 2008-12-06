package fj.function;

import fj.F;
import fj.F2;
import static fj.Function.curry;

/**
 * Curried string functions.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public class Strings {
  private Strings() {
    throw new UnsupportedOperationException();
  }

  /**
   * A curried version of {@link String#isEmpty()}.
   */
  public static final F<String, Boolean> isEmpty = new F<String, Boolean>() {
    public Boolean f(final String s) {
      return s.length() == 0;
    }
  };

  /**
   * A curried version of {@link String#length()}.
   */
  public static final F<String, Integer> length = new F<String, Integer>() {
    public Integer f(final String s) {
      return s.length();
    }
  };

  /**
   * A curried version of {@link String#contains(CharSequence)}.
   * The function returns true if the second argument contains the first.
   */
  public static final F<String, F<String, Boolean>> contains = curry(new F2<String, String, Boolean>() {
    public Boolean f(final String s1, final String s2) {
      return s2.contains(s1);
    }
  });

  /**
   * A curried version of {@link String#matches(String)}.
   * The function returns true if the second argument matches the first.
   */
  public static final F<String, F<String, Boolean>> matches = curry(new F2<String, String, Boolean>() {
    public Boolean f(final String s1, final String s2) {
      return s2.matches(s1);
    }
  });

}
