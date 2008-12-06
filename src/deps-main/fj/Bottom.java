package fj;

import static fj.data.List.asString;
import fj.pre.Show;

/**
 * Represents the bottom _|_ value.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class Bottom {
  private Bottom() {
    throw new UnsupportedOperationException();
  }

  /**
   * Returns an error to represent undefinedness in a computation.
   *
   * @return An error to represent undefinedness in a computation.
   */
  public static Error undefined() {
    return error("undefined");
  }

  /**
   * Returns an error to represent undefinedness in a computation with early failure using the given
   * message.
   *
   * @param s The message to fail with.
   * @return An error to represent undefinedness in a computation with early failure using the given
   * message.
   */
  public static Error error(final String s) {
    throw new Error(s);
  }

  /**
   * Represents a deconstruction failure that was non-exhaustive.
   *
   * @param a The value being deconstructed.
   * @param sa The rendering for the value being deconstructed.
   * @return A deconstruction failure that was non-exhaustive.
   */
  public static <A> Error decons(final A a, final Show<A> sa) {
    return error("Deconstruction failure on type " + a.getClass() + " with value " + asString(sa.show(a)));
  }

  /**
   * Represents a deconstruction failure that was non-exhaustive.
   *
   * @param c The type being deconstructed.
   * @return A deconstruction failure that was non-exhaustive.
   */
  public static <A> Error decons(final java.lang.Class<A> c) {
    return error("Deconstruction failure on type " + c);
  }

  /**
   * A function that returns the <code>toString</code> for a throwable.
   *
   * @return A function that returns the <code>toString</code> for a throwable.
   */
  public static <T extends Throwable> F<T, String> eToString() {
    return new F<T, String>() {
      public String f(final Throwable t) {
        return t.toString();
      }
    };
  }

  /**
   * A function that returns the <code>getMessage</code> for a throwable.
   *
   * @return A function that returns the <code>getMessage</code> for a throwable.
   */
  public static <T extends Throwable> F<T, String> eMessage() {
    return new F<T, String>() {
      public String f(final Throwable t) {
        return t.getMessage();
      }
    };
  }
}
