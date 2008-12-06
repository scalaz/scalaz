package fj.parser;

import fj.F;
import fj.F2;
import static fj.Function.curry;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * A parse result made up of a value (A) and the remainder of the parse input (I).
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class Result<I, A> implements Iterable<A> {
  private final I i;
  private final A a;

  private Result(final I i, final A a) {
    this.i = i;
    this.a = a;
  }

  /**
   * The remainder of the parse input.
   *
   * @return The remainder of the parse input.
   */
  public I rest() {
    return i;
  }

  /**
   * The parsed value.
   *
   * @return The parsed value.
   */
  public A value() {
    return a;
  }

  /**
   * Maps the given function across the remainder of the parse input.
   *
   * @param f The function to map with.
   * @return A result with a different parse input.
   */
  public <J> Result<J, A> mapRest(final F<I, J> f) {
    return result(f.f(i), a);
  }

  /**
   * First-class function mapping across the remainder of the parse input.
   *
   * @return A first-class function mapping across the remainder of the parse input.
   */
  public <J> F<F<I, J>, Result<J, A>> mapRest() {
    return new F<F<I, J>, Result<J, A>>() {
      public Result<J, A> f(final F<I, J> f) {
        return mapRest(f);
      }
    };
  }

  /**
   * Maps the given function across the parse value.
   *
   * @param f The function to map with.
   * @return A result with a different parse value.
   */
  public <B> Result<I, B> mapValue(final F<A, B> f) {
    return result(i, f.f(a));
  }

  /**
   * First-class function mapping across the parse value.
   *
   * @return A first-class function mapping across the parse value.
   */
  public <B> F<F<A, B>, Result<I, B>> mapValue() {
    return new F<F<A, B>, Result<I, B>>() {
      public Result<I, B> f(final F<A, B> f) {
        return mapValue(f);
      }
    };
  }

  /**
   * A bifunctor map across both the remainder of the parse input and the parse value.
   *
   * @param f The function to map the remainder of the parse input with.
   * @param g The function to map the parse value with.
   * @return A result with a different parse input and parse value.
   */
  public <B, J> Result<J, B> bimap(final F<I, J> f, final F<A, B> g) {
    return mapRest(f).mapValue(g);
  }

  /**
   * First-class bifunctor map.
   *
   * @return A first-class bifunctor map.
   */
  public <B, J> F<F<I, J>, F<F<A, B>, Result<J, B>>> bimap() {
    return curry(new F2<F<I, J>, F<A, B>, Result<J, B>>() {
      public Result<J, B> f(final F<I, J> f, final F<A, B> g) {
        return bimap(f, g);
      }
    });
  }

  /**
   * Returns an iterator over the parse value. This method exists to permit the use in a <code>for</code>-each loop.
   *
   * @return An iterator over the parse value.
   */
  public Iterator<A> iterator() {
    return new Iterator<A>() {
      private boolean r;

      public boolean hasNext() {
        return !r;
      }

      public A next() {
        if(r)
          throw new NoSuchElementException();
        else {
          r = true;
          return a;
        }
      }

      public void remove() {
        throw new UnsupportedOperationException();
      }
    };
  }

  /**
   * Construct a result with the given remainder of the parse input and parse value.
   *
   * @param i The remainder of the parse input.
   * @param a The parse value.
   * @return A result with the given remainder of the parse input and parse value.
   */
  public static <A, I> Result<I, A> result(final I i, final A a) {
    return new Result<I, A>(i, a);
  }

  /**
   * First-class construction of a result.
   *
   * @return A first-class function for construction of a result.
   */
  public static <A, I> F<I, F<A, Result<I, A>>> result() {
    return curry(new F2<I, A, Result<I, A>>() {
      public Result<I, A> f(final I i, final A a) {
        return result(i, a);
      }
    });
  }
}
