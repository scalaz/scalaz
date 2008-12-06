package fj;

import static fj.Unit.unit;

/**
 * Represents a side-effect.
 * 
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public interface Effect<A> {
  void e(A a);

  /**
   * A projection of an effect. The methods defined on a projection may belong on an effect,
   * however, this would disallow the use of {@link Effect} to be used with Java 7 closure syntax.
   */
  @SuppressWarnings({"InnerClassOfInterface"})
  final class Projection {
    private Projection() {
      throw new UnsupportedOperationException();
    }

    /**
     * Returns an effect for the given function.
     *
     * @param f The function to produce the effort with.
     * @return The effect using the given function.
     */
    public static <A> Effect<A> f(final F<A, Unit> f) {
      return new Effect<A>() {
        public void e(final A a) {
          f.f(a);
        }
      };
    }

    /**
     * Returns a function for the given effect.
     *
     * @param e The effect to produce the function with.
     * @return The function using the given effect.
     */
    public static <A> F<A, Unit> e(final Effect<A> e) {
      return new F<A, Unit>() {
        public Unit f(final A a) {
          e.e(a);
          return unit();
        }
      };
    }

    /**
     * A contra-variant functor on effect.
     *
     * @param e The effect to map over.
     * @param f The function to map over the effect.
     * @return An effect after a contra-variant map.
     */
    public static <A, B> Effect<B> comap(final Effect<A> e, final F<B, A> f) {
      return new Effect<B>() {
        public void e(final B b) {
          e.e(f.f(b));
        }
      };
    }
  }
}
