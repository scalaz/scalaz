package fj.data;

import static fj.data.Option.some;
import static fj.data.Stream.iterableStream;
import fj.F;
import fj.F2;
import fj.F3;
import fj.P1;
import fj.P2;
import static fj.Function.curry;
import static fj.Function.identity;

import java.util.Iterator;

/**
 * A wrapper for Iterable that equips it with some useful functions.
 */
public final class IterableW<A> implements Iterable<A> {

  private final Iterable<A> i;

  private IterableW(final Iterable<A> i) {
    this.i = i;
  }

  /**
   * Wraps the given iterable.
   *
   * @param a The iterable to wrap.
   * @return An iterable equipped with some useful functions.
   */
  public static <A> IterableW<A> wrap(final Iterable<A> a) {
    return new IterableW<A>(a);
  }

  /**
   * Provides a function that wraps the given iterable.
   *
   * @return A function that returns the given iterable, wrapped.
   */
  public static <A, T extends Iterable<A>> F<T, IterableW<A>> wrap() {
    return new F<T, IterableW<A>>() {
      public IterableW<A> f(final T a) {
        return wrap(a);
      }
    };
  }

  /**
   * Returns an Iterable that completely preserves the argument. The unit function for Iterables.
   *
   * @param a A value to preserve in an Iterable.
   * @return An Iterable that yields the argument when iterated over.
   */
  public static <A> IterableW<A> iterable(final A a) {
    return wrap(some(a));
  }

  /**
   * Wraps a given function's return value in a Iterable.
   * The Kleisli arrow for Iterables.
   *
   * @param f The function whose return value to wrap in a Iterable.
   * @return The equivalent function whose return value is iterable.
   */
  public static <A, B> F<A, IterableW<B>> iterable(final F<A, B> f) {
    return new F<A, IterableW<B>>() {
      public IterableW<B> f(final A a) {
        return iterable(f.f(a));
      }
    };
  }

  /**
   * Provides a transformation from a function to a Iterable-valued function that is equivalent to it.
   * The first-class Kleisli arrow for Iterables.
   *
   * @return A transformation from a function to the equivalent Iterable-valued function.
   */
  public static <A, B> F<F<A, B>, F<A, IterableW<B>>> arrow() {
    return new F<F<A, B>, F<A, IterableW<B>>>() {
      public F<A, IterableW<B>> f(final F<A, B> f) {
        return iterable(f);
      }
    };
  }

  /**
   * Binds the given function across the wrapped Iterable with a final join.
   *
   * @param f A function to bind across the Iterable.
   * @return an iterable result of binding the given function over the wrapped Iterable.
   */
  public <B, T extends Iterable<B>> IterableW<B> bind(final F<A, T> f) {
    return wrap(iterableStream(this).bind(new F<A, Stream<B>>() {
      public Stream<B> f(final A a) {
        return iterableStream(f.f(a));
      }
    }));
  }

  /**
   * Performs function application within an iterable (applicative functor pattern).
   *
   * @param f The iterable function to apply.
   * @return A new iterable after applying the given iterable function to the wrapped iterable.
   */
  public <B> IterableW<B> apply(final Iterable<F<A, B>> f) {
    return wrap(f).bind(new F<F<A, B>, Iterable<B>>() {
      public Iterable<B> f(final F<A, B> f) {
        return map(f);
      }
    });
  }

  /**
   * Binds the given function to the values in the given iterables with a final join.
   *
   * @param a A given iterable to bind the given function with.
   * @param b A given iterable to bind the given function with.
   * @param f The function to apply to the values in the given iterables.
   * @return A new iterable after performing the map, then final join.
   */
  public static <A, B, C> IterableW<C> bind(final Iterable<A> a, final Iterable<B> b, final F<A, F<B, C>> f) {
    return wrap(b).apply(wrap(a).map(f));
  }

  /**
   * Promotes a function of arity-2 to a function on iterables.
   *
   * @param f The function to promote.
   * @return A function of arity-2 promoted to map over iterables.
   */
  public static <A, B, C> F<Iterable<A>, F<Iterable<B>, IterableW<C>>> liftM2(final F<A, F<B, C>> f) {
    return curry(new F2<Iterable<A>, Iterable<B>, IterableW<C>>() {
      public IterableW<C> f(final Iterable<A> ca, final Iterable<B> cb) {
        return bind(ca, cb, f);
      }
    });
  }

  /**
   * Performs a bind across each element of all iterables of an iterable, collecting the values in an iterable.
   * This implementation is strict and requires O(n) stack space.
   *
   * @param as The iterable of iterables to transform.
   * @return A iterable of iterables containing the results of the bind operations across all given iterables.
   */
  public static <A, T extends Iterable<A>> IterableW<IterableW<A>> sequence(final Iterable<T> as) {
    final Stream<T> ts = iterableStream(as);
    return ts.isEmpty() ? iterable(wrap(Option.<A>none())) : wrap(ts.head()).bind(new F<A, Iterable<IterableW<A>>>() {
      public Iterable<IterableW<A>> f(final A a) {
        return sequence(ts.tail().map(IterableW.<T, Stream<T>>wrap())._1()).bind(new F<IterableW<A>, Iterable<IterableW<A>>>() {
          public Iterable<IterableW<A>> f(final IterableW<A> as) {
            return iterable(wrap(Stream.cons(a, new P1<Stream<A>>() {
              public Stream<A> _1() {
                return iterableStream(as);
              }
            })));
          }
        });
      }
    });
  }

  /**
   * The first-class bind function over Iterable.
   * Returns a function that binds a given function across a given iterable.
   *
   * @return a function that binds a given function across a given iterable.
   */
  public static <A, B, T extends Iterable<B>> F<IterableW<A>, F<F<A, T>, IterableW<B>>> bind() {
    return new F<IterableW<A>, F<F<A, T>, IterableW<B>>>() {
      public F<F<A, T>, IterableW<B>> f(final IterableW<A> a) {
        return new F<F<A, T>, IterableW<B>>() {
          public IterableW<B> f(final F<A, T> f) {
            return a.bind(f);
          }
        };
      }
    };
  }

  /**
   * Joins an Iterable of Iterables into a single Iterable.
   *
   * @param as An Iterable of Iterables to join.
   * @return the joined Iterable.
   */
  public static <A, T extends Iterable<A>> IterableW<A> join(final Iterable<T> as) {
    final F<T, T> id = identity();
    return wrap(as).bind(id);
  }

  /**
   * Returns a function that joins an Iterable of Iterables into a single Iterable.
   *
   * @return a function that joins an Iterable of Iterables into a single Iterable.
   */
  public static <A, T extends Iterable<A>> F<Iterable<T>, IterableW<A>> join() {
    return new F<Iterable<T>, IterableW<A>>() {
      public IterableW<A> f(final Iterable<T> a) {
        return join(a);
      }
    };
  }

  /**
   * Maps a given function across the wrapped Iterable.
   *
   * @param f A function to map across the wrapped Iterable.
   * @return An Iterable of the results of mapping the given function across the wrapped Iterable.
   */
  public <B> IterableW<B> map(final F<A, B> f) {
    return bind(iterable(f));
  }

  /**
   * Returns a function that promotes any function so that it operates on Iterables.
   *
   * @return a function that promotes any function so that it operates on Iterables.
   */
  public static <A, B> F<F<A, B>, F<IterableW<A>, IterableW<B>>> map() {
    return new F<F<A, B>, F<IterableW<A>, IterableW<B>>>() {
      public F<IterableW<A>, IterableW<B>> f(final F<A, B> f) {
        return new F<IterableW<A>, IterableW<B>>() {
          public IterableW<B> f(final IterableW<A> a) {
            return a.map(f);
          }
        };
      }
    };
  }

  /**
   * The catamorphism for Iterables, implemented as a left fold.
   *
   * @param f The function with which to fold the wrapped iterable.
   * @param z The base case value of the destination type, applied first (leftmost) to the fold.
   * @return The result of the catamorphism.
   */
  public <B> B foldLeft(final F<B, F<A, B>> f, final B z) {
    B p = z;
    for (final A x : this) {
      p = f.f(p).f(x);
    }
    return p;
  }

  /**
   * The catamorphism for Iterables, implemented as a right fold.
   *
   * @param f The function with which to fold the wrapped iterable.
   * @param z The base case value of the destination type, applied last (rightmost) to the fold.
   * @return The result of the catamorphism.
   */
  public <B> B foldRight(final F2<A, B, B> f, final B z) {
    final F<B, B> id = identity();
    return foldLeft(curry(new F3<F<B, B>, A, B, B>() {
      public B f(final F<B, B> k, final A a, final B b) {
        return k.f(f.f(a, b));
      }
    }), id).f(z);
  }

  /**
   * Returns an iterator for this iterable.
   *
   * @return an iterator for this iterable.
   */
  public Iterator<A> iterator() {
    return i.iterator();
  }

  /**
   * Zips this iterable with the given iterable of functions, applying each function in turn to the
   * corresponding element in this iterable to produce a new iterable. The iteration is normalised
   * so that it ends when one of the iterators is exhausted.
   *
   * @param fs The iterable of functions to apply to this iterable.
   * @return A new iterable with the results of applying the functions to this iterable.
   */
  public <B> IterableW<B> zapp(final Iterable<F<A, B>> fs) {
    return wrap(iterableStream(this).zapp(iterableStream(fs)));
  }

  /**
   * Zips this iterable with the given iterable using the given function to produce a new iterable. If
   * this iterable and the given iterable have different lengths, then the longer iterable is normalised
   * so this function never fails.
   *
   * @param bs The iterable to zip this iterable with.
   * @param f  The function to zip this iterable and the given iterable with.
   * @return A new iterable with a length the same as the shortest of this iterable and the given
   *         iterable.
   */
  public <B, C> Iterable<C> zipWith(final Iterable<B> bs, final F<A, F<B, C>> f) {
    return wrap(iterableStream(this).zipWith(iterableStream(bs), f));
  }

  /**
   * Zips this iterable with the given iterable to produce a iterable of pairs. If this iterable and the
   * given iterable have different lengths, then the longer iterable is normalised so this function
   * never fails.
   *
   * @param bs The iterable to zip this iterable with.
   * @return A new iterable with a length the same as the shortest of this iterable and the given
   *         iterable.
   */
  public <B> Iterable<P2<A, B>> zip(final Iterable<B> bs) {
    return wrap(iterableStream(this).zip(iterableStream(bs)));
  }

  /**
   * Zips this iterable with the index of its element as a pair.
   *
   * @return A new iterable with the same length as this iterable.
   */
  public Iterable<P2<A, Integer>> zipIndex() {
    return wrap(iterableStream(this).zipIndex());
  }
}
