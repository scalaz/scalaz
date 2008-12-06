package fj;

/**
 * A product-6.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
@SuppressWarnings({"UnnecessaryFullyQualifiedName"})
public abstract class P6<A, B, C, D, E, F> {
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
   * Access the third element of the product.
   *
   * @return The third element of the product.
   */
  public abstract C _3();

  /**
   * Access the fourth element of the product.
   *
   * @return The fourth element of the product.
   */
  public abstract D _4();

  /**
   * Access the fifth element of the product.
   *
   * @return The fifth element of the product.
   */
  public abstract E _5();

  /**
   * Access the sixth element of the product.
   *
   * @return The sixth element of the product.
   */
  public abstract F _6();

  /**
   * Map the first element of the product.
   *
   * @param f The function to map with.
   * @return A product with the given function applied.
   */
  public <X> P6<X, B, C, D, E, F> map1(final fj.F<A, X> f) {
    return new P6<X, B, C, D, E, F>() {
      public X _1() {
        return f.f(P6.this._1());
      }

      public B _2() {
        return P6.this._2();
      }

      public C _3() {
        return P6.this._3();
      }

      public D _4() {
        return P6.this._4();
      }

      public E _5() {
        return P6.this._5();
      }

      public F _6() {
        return P6.this._6();
      }
    };
  }

  /**
   * Map the second element of the product.
   *
   * @param f The function to map with.
   * @return A product with the given function applied.
   */
  public <X> P6<A, X, C, D, E, F> map2(final fj.F<B, X> f) {
    return new P6<A, X, C, D, E, F>() {
      public A _1() {
        return P6.this._1();
      }

      public X _2() {
        return f.f(P6.this._2());
      }

      public C _3() {
        return P6.this._3();
      }

      public D _4() {
        return P6.this._4();
      }

      public E _5() {
        return P6.this._5();
      }

      public F _6() {
        return P6.this._6();
      }
    };
  }

  /**
   * Map the third element of the product.
   *
   * @param f The function to map with.
   * @return A product with the given function applied.
   */
  public <X> P6<A, B, X, D, E, F> map3(final fj.F<C, X> f) {
    return new P6<A, B, X, D, E, F>() {
      public A _1() {
        return P6.this._1();
      }

      public B _2() {
        return P6.this._2();
      }

      public X _3() {
        return f.f(P6.this._3());
      }

      public D _4() {
        return P6.this._4();
      }

      public E _5() {
        return P6.this._5();
      }

      public F _6() {
        return P6.this._6();
      }
    };
  }

  /**
   * Map the fourth element of the product.
   *
   * @param f The function to map with.
   * @return A product with the given function applied.
   */
  public <X> P6<A, B, C, X, E, F> map4(final fj.F<D, X> f) {
    return new P6<A, B, C, X, E, F>() {
      public A _1() {
        return P6.this._1();
      }

      public B _2() {
        return P6.this._2();
      }

      public C _3() {
        return P6.this._3();
      }

      public X _4() {
        return f.f(P6.this._4());
      }

      public E _5() {
        return P6.this._5();
      }

      public F _6() {
        return P6.this._6();        
      }
    };
  }

  /**
   * Map the fifth element of the product.
   *
   * @param f The function to map with.
   * @return A product with the given function applied.
   */
  public <X> P6<A, B, C, D, X, F> map5(final fj.F<E, X> f) {
    return new P6<A, B, C, D, X, F>() {
      public A _1() {
        return P6.this._1();
      }

      public B _2() {
        return P6.this._2();
      }

      public C _3() {
        return P6.this._3();
      }

      public D _4() {
        return P6.this._4();
      }

      public X _5() {
        return f.f(P6.this._5());
      }

      public F _6() {
        return P6.this._6();
      }
    };
  }

  /**
   * Map the sixth element of the product.
   *
   * @param f The function to map with.
   * @return A product with the given function applied.
   */
  public <X> P6<A, B, C, D, E, X> map6(final fj.F<F, X> f) {
    return new P6<A, B, C, D, E, X>() {
      public A _1() {
        return P6.this._1();
      }

      public B _2() {
        return P6.this._2();
      }

      public C _3() {
        return P6.this._3();
      }

      public D _4() {
        return P6.this._4();
      }

      public E _5() {
        return P6.this._5();
      }

      public X _6() {
        return f.f(P6.this._6());
      }
    };
  }

  /**
   * Returns a function that returns the first element of a product.
   *
   * @return A function that returns the first element of a product.
   */
  public static <A, B, C, D, E, F$> fj.F<P6<A, B, C, D, E, F$>, A> __1() {
    return new fj.F<P6<A, B, C, D, E, F$>, A>() {
      public A f(final P6<A, B, C, D, E, F$> p) {
        return p._1();
      }
    };
  }

  /**
   * Returns a function that returns the second element of a product.
   *
   * @return A function that returns the second element of a product.
   */
  public static <A, B, C, D, E, F$> fj.F<P6<A, B, C, D, E, F$>, B> __2() {
    return new fj.F<P6<A, B, C, D, E, F$>, B>() {
      public B f(final P6<A, B, C, D, E, F$> p) {
        return p._2();
      }
    };
  }

  /**
   * Returns a function that returns the third element of a product.
   *
   * @return A function that returns the third element of a product.
   */
  public static <A, B, C, D, E, F$> fj.F<P6<A, B, C, D, E, F$>, C> __3() {
    return new fj.F<P6<A, B, C, D, E, F$>, C>() {
      public C f(final P6<A, B, C, D, E, F$> p) {
        return p._3();
      }
    };
  }

  /**
   * Returns a function that returns the fourth element of a product.
   *
   * @return A function that returns the fourth element of a product.
   */
  public static <A, B, C, D, E, F$> fj.F<P6<A, B, C, D, E, F$>, D> __4() {
    return new fj.F<P6<A, B, C, D, E, F$>, D>() {
      public D f(final P6<A, B, C, D, E, F$> p) {
        return p._4();
      }
    };
  }

  /**
   * Returns a function that returns the fifth element of a product.
   *
   * @return A function that returns the fifth element of a product.
   */
  public static <A, B, C, D, E, F$> fj.F<P6<A, B, C, D, E, F$>, E> __5() {
    return new fj.F<P6<A, B, C, D, E, F$>, E>() {
      public E f(final P6<A, B, C, D, E, F$> p) {
        return p._5();
      }
    };
  }

  /**
   * Returns a function that returns the sixth element of a product.
   *
   * @return A function that returns the sixth element of a product.
   */
  public static <A, B, C, D, E, F$> fj.F<P6<A, B, C, D, E, F$>, F$> __6() {
    return new fj.F<P6<A, B, C, D, E, F$>, F$>() {
      public F$ f(final P6<A, B, C, D, E, F$> p) {
        return p._6();
      }
    };
  }
}
