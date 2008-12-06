package fj;

import static fj.Function.compose;
import static fj.Function.join;
import static fj.Function.curry;

/**
 * A product-2.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public abstract class P2<A, B> {
  /**
   * Access the first element of the product.
   *
   * @return The first element of the product.
   */
  public abstract A _1();

  /**
   * Access the second element of the product.
   *
   * @return The second element of the product.
   */
  public abstract B _2();

  /**
   * Swaps the elements around in this product.
   *
   * @return A new product-2 with the elements swapped.
   */
  public P2<B, A> swap() {
    return new P2<B, A>() {
      public B _1() {
        return P2.this._2();
      }

      public A _2() {
        return P2.this._1();
      }
    };
  }

  /**
   * Map the first element of the product.
   *
   * @param f The function to map with.
   * @return A product with the given function applied.
   */
  public <X> P2<X, B> map1(final F<A, X> f) {
    return new P2<X, B>() {
      public X _1() {
        return f.f(P2.this._1());
      }

      public B _2() {
        return P2.this._2();
      }
    };
  }

  /**
   * Map the second element of the product.
   *
   * @param f The function to map with.
   * @return A product with the given function applied.
   */
  public <X> P2<A, X> map2(final F<B, X> f) {
    return new P2<A, X>() {
      public A _1() {
        return P2.this._1();
      }

      public X _2() {
        return f.f(P2.this._2());
      }
    };
  }

  /**
   * Split this product between two argument functions and combine their output.
   *
   * @param f A function that will map the first element of this product.
   * @param g A function that will map the second element of this product.
   * @return A new product with the first function applied to the second element
   *         and the second function applied to the second element.
   */
  public <C, D> P2<C, D> split(final F<A, C> f, final F<B, D> g) {
    final F<P2<A, D>, P2<C, D>> ff = map1_(f);
    final F<P2<A, B>, P2<A, D>> gg = map2_(g);
    return compose(ff, gg).f(this);
  }

  /**
   * A first-class version of the split function.
   *
   * @param f A function that will map the first element of the given product.
   * @param g A function that will map the second element of the given product.
   * @return A function that splits a given product between the two given functions and combines their output.
   */
  public static <A, B, C, D> F<P2<A, B>, P2<C, D>> split_(final F<A, C> f, final F<B, D> g) {
    return new F<P2<A, B>, P2<C, D>>() {
      public P2<C, D> f(final P2<A, B> p) {
        return p.split(f, g);
      }
    };
  }

  /**
   * Promotes a function so that it maps the first element of a product.
   *
   * @param f The function to promote.
   * @return The given function, promoted to map the first element of products.
   */
  public static <A, B, X> F<P2<A, B>, P2<X, B>> map1_(final F<A, X> f) {
    return new F<P2<A, B>, P2<X, B>>() {
      public P2<X, B> f(final P2<A, B> p) {
        return p.map1(f);
      }
    };
  }

  /**
   * Promotes a function so that it maps the second element of a product.
   *
   * @param f The function to promote.
   * @return The given function, promoted to map the second element of products.
   */
  public static <A, B, X> F<P2<A, B>, P2<A, X>> map2_(final F<B, X> f) {
    return new F<P2<A, B>, P2<A, X>>() {
      public P2<A, X> f(final P2<A, B> p) {
        return p.map2(f);
      }
    };
  }

  /**
   * Sends the given input value to both argument functions and combines their output.
   *
   * @param f A function to receive an input value.
   * @param g A function to receive an input value.
   * @param b An input value to send to both functions.
   * @return The product of the two functions applied to the input value.
   */
  public static <B, C, D> P2<C, D> fanout(final F<B, C> f, final F<B, D> g, final B b) {
    return join(P.<B, B>p2()).f(b).split(f, g);
  }

  /**
   * Maps the given function across both the elements of the given product.
   *
   * @param f A function to map over a product.
   * @param p A product over which to map.
   * @return A new product with the given function applied to both elements.
   */
  public static <A, B> P2<B, B> map(final F<A, B> f, final P2<A, A> p) {
    return p.split(f, f);
  }

  /**
   * Returns a curried form of {@link #swap()}.
   *
   * @return A curried form of {@link #swap()}.
   */
  public static <A, B> F<P2<A, B>, P2<B, A>> swap_() {
    return new F<P2<A, B>, P2<B, A>>() {
      public P2<B, A> f(P2<A, B> p) {
        return p.swap();
      }
    };
  }

  /**
   * Returns a function that returns the first element of a product.
   *
   * @return A function that returns the first element of a product.
   */
  public static <A, B> F<P2<A, B>, A> __1() {
    return new F<P2<A, B>, A>() {
      public A f(final P2<A, B> p) {
        return p._1();
      }
    };
  }

  /**
   * Returns a function that returns the second element of a product.
   *
   * @return A function that returns the second element of a product.
   */
  public static <A, B> F<P2<A, B>, B> __2() {
    return new F<P2<A, B>, B>() {
      public B f(final P2<A, B> p) {
        return p._2();
      }
    };
  }

  /**
   * Transforms a curried function of arity-2 to a function of a product-2
   *
   * @param f a curried function of arity-2 to transform into a function of a product-2
   * @return The function, transformed to operate on on a product-2
   */
  public static <A, B, C> F<P2<A, B>, C> untuple(final F<A, F<B, C>> f) {
    return new F<P2<A, B>, C>() {
      public C f(final P2<A, B> p) {
        return f.f(p._1()).f(p._2());
      }
    };
  }

  /**
   * Transforms an uncurried function of arity-2 to a function of a product-2
   *
   * @param f an uncurried function of arity-2 to transform into a function of a product-2
   * @return The function, transformed to operate on on a product-2
   */
  public static <A, B, C> F<P2<A, B>, C> untuple(final F2<A, B, C> f) {
    return untuple(curry(f));
  }

}
