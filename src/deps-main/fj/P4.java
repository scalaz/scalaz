package fj;

/**
 * A product-4.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public abstract class P4<A, B, C, D> {
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
   * Map the first element of the product.
   *
   * @param f The function to map with.
   * @return A product with the given function applied.
   */
  public <X> P4<X, B, C, D> map1(final F<A, X> f) {
    return new P4<X, B, C, D>() {
      public X _1() {
        return f.f(P4.this._1());
      }

      public B _2() {
        return P4.this._2();
      }

      public C _3() {
        return P4.this._3();
      }

      public D _4() {
        return P4.this._4();
      }
    };
  }

  /**
   * Map the second element of the product.
   *
   * @param f The function to map with.
   * @return A product with the given function applied.
   */
  public <X> P4<A, X, C, D> map2(final F<B, X> f) {
    return new P4<A, X, C, D>() {
      public A _1() {
        return P4.this._1();
      }

      public X _2() {
        return f.f(P4.this._2());
      }

      public C _3() {
        return P4.this._3();
      }

      public D _4() {
        return P4.this._4();
      }
    };
  }

  /**
   * Map the third element of the product.
   *
   * @param f The function to map with.
   * @return A product with the given function applied.
   */
  public <X> P4<A, B, X, D> map3(final F<C, X> f) {
    return new P4<A, B, X, D>() {
      public A _1() {
        return P4.this._1();
      }

      public B _2() {
        return P4.this._2();
      }

      public X _3() {
        return f.f(P4.this._3());
      }

      public D _4() {
        return P4.this._4();
      }
    };
  }

  /**
   * Map the fourth element of the product.
   *
   * @param f The function to map with.
   * @return A product with the given function applied.
   */
  public <X> P4<A, B, C, X> map4(final F<D, X> f) {
    return new P4<A, B, C, X>() {
      public A _1() {
        return P4.this._1();
      }

      public B _2() {
        return P4.this._2();
      }

      public C _3() {
        return P4.this._3();
      }

      public X _4() {
        return f.f(P4.this._4());
      }
    };
  }

  /**
   * Returns a function that returns the first element of a product.
   *
   * @return A function that returns the first element of a product.
   */
  public static <A, B, C, D> F<P4<A, B, C, D>, A> __1() {
    return new F<P4<A, B, C, D>, A>() {
      public A f(final P4<A, B, C, D> p) {
        return p._1();
      }
    };
  }

  /**
   * Returns a function that returns the second element of a product.
   *
   * @return A function that returns the second element of a product.
   */
  public static <A, B, C, D> F<P4<A, B, C, D>, B> __2() {
    return new F<P4<A, B, C, D>, B>() {
      public B f(final P4<A, B, C, D> p) {
        return p._2();
      }
    };
  }

  /**
   * Returns a function that returns the third element of a product.
   *
   * @return A function that returns the third element of a product.
   */
  public static <A, B, C, D> F<P4<A, B, C, D>, C> __3() {
    return new F<P4<A, B, C, D>, C>() {
      public C f(final P4<A, B, C, D> p) {
        return p._3();
      }
    };
  }

  /**
   * Returns a function that returns the fourth element of a product.
   *
   * @return A function that returns the fourth element of a product.
   */
  public static <A, B, C, D> F<P4<A, B, C, D>, D> __4() {
    return new F<P4<A, B, C, D>, D>() {
      public D f(final P4<A, B, C, D> p) {
        return p._4();
      }
    };
  }
}
