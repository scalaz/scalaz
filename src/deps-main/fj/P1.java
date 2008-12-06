package fj;

import fj.data.List;
import fj.data.Stream;
import fj.data.Array;

/**
 * A product-1. Also, the identity monad.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public abstract class P1<A> {
  /**
   * Access the first element of the product.
   *
   * @return The first element of the product.
   */
  public abstract A _1();

  /**
   * Map the element of the product.
   *
   * @param f The function to map with.
   * @return A product with the given function applied.
   */
  public <X> P1<X> map(final F<A, X> f) {
    return new P1<X>() {
      public X _1() {
        return f.f(P1.this._1());
      }
    };
  }

  /**
   * Returns a function that returns the first element of a product.
   *
   * @return A function that returns the first element of a product.
   */
  public static <A> F<P1<A>, A> __1() {
    return new F<P1<A>, A>() {
      public A f(final P1<A> p) {
        return p._1();
      }
    };
  }

  /**
   * Promote any function to a transformation between P1s.
   *
   * @param f A function to promote to a transformation between P1s.
   * @return A function promoted to operate on P1s.
   */
  public static <A, B> F<P1<A>, P1<B>> fmap(final F<A, B> f) {
    return new F<P1<A>, P1<B>>() {
      public P1<B> f(final P1<A> a) {
        return a.map(f);
      }
    };
  }

  /**
   * Binds the given function to the value in a product-1 with a final join.
   *
   * @param a A value in a product-1 to which to apply a function.
   * @param f A function to apply to the value in a product-1.
   * @return The result of applying the given function to the value of given product-1.
   */
  public static <A, B> P1<B> bind(final P1<A> a, final F<A, P1<B>> f) {
    return new P1<B>() {
      public B _1() {
        return f.f(a._1())._1();
      }
    };
  }

  /**
   * Promotes the given function so that it returns its value in a P1.
   *
   * @param f A function to have its result wrapped in a P1.
   * @return A function whose result is wrapped in a P1.
   */
  public static <A, B> F<A, P1<B>> curry(final F<A, B> f) {
    return new F<A, P1<B>>() {
      public P1<B> f(final A a) {
        return new P1<B>() {
          public B _1() {
            return f.f(a);
          }
        };
      }
    };
  }

  /**
   * Performs function application within a P1 (applicative functor pattern).
   *
   * @param ca The P1 to which to apply a function.
   * @param cf The P1 function to apply.
   * @return A new P1 after applying the given P1 function to the first argument.
   */
  public static <A, B> P1<B> apply(final P1<A> ca, final P1<F<A, B>> cf) {
    return bind(cf, new F<F<A, B>, P1<B>>() {
      public P1<B> f(final F<A, B> f) {
        return fmap(f).f(ca);
      }
    });
  }

  /**
   * Binds the given function to the values in the given P1s with a final join.
   *
   * @param ca A given P1 to bind the given function with.
   * @param cb A given P1 to bind the given function with.
   * @param f  The function to apply to the values in the given P1s.
   * @return A new P1 after performing the map, then final join.
   */
  public static <A, B, C> P1<C> bind(final P1<A> ca, final P1<B> cb, final F<A, F<B, C>> f) {
    return apply(cb, fmap(f).f(ca));
  }

  /**
   * Joins a P1 of a P1 with a bind operation.
   *
   * @param a The P1 of a P1 to join.
   * @return A new P1 that is the join of the given P1.
   */
  public static <A> P1<A> join(final P1<P1<A>> a) {
    return bind(a, Function.<P1<A>>identity());
  }

  /**
   * Promotes a function of arity-2 to a function on P1s.
   *
   * @param f The function to promote.
   * @return A function of arity-2 promoted to map over P1s.
   */
  public static <A, B, C> F<P1<A>, F<P1<B>, P1<C>>> liftM2(final F<A, F<B, C>> f) {
    return Function.curry(new F2<P1<A>, P1<B>, P1<C>>() {
      public P1<C> f(final P1<A> pa, final P1<B> pb) {
        return bind(pa, pb, f);
      }
    });
  }

  /**
   * Turns a List of P1s into a single P1 of a List.
   *
   * @param as The list of P1s to transform.
   * @return A single P1 for the given List.
   */
  public static <A> P1<List<A>> sequence(final List<P1<A>> as) {
    return as.foldRight(liftM2(List.<A>cons()), P.p(List.<A>nil()));
  }

  /**
   * A first-class version of the sequence method for lists of P1s.
   *
   * @return A function from a List of P1s to a single P1 of a List.
   */
  public static <A> F<List<P1<A>>, P1<List<A>>> sequenceList() {
    return new F<List<P1<A>>, P1<List<A>>>() {
      public P1<List<A>> f(final List<P1<A>> as) {
        return sequence(as);
      }
    };
  }

  /**
   * Turns a stream of P1s into a single P1 of a stream.
   *
   * @param as The stream of P1s to transform.
   * @return A single P1 for the given stream.
   */
  public static <A> P1<Stream<A>> sequence(final Stream<P1<A>> as) {
    return as.foldRight(liftM2(Stream.<A>cons()), P.p(Stream.<A>nil()));
  }

  /**
   * Turns an array of P1s into a single P1 of an array.
   *
   * @param as The array of P1s to transform.
   * @return A single P1 for the given array.
   */
  public static <A> P1<Array<A>> sequence(final Array<P1<A>> as) {
    return new P1<Array<A>>() {
      public Array<A> _1() {
        return as.map(P1.<A>__1());
      }
    };
  }

}
