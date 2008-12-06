package fj;

/**
 * Functions across products.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class P {
  private P() {
    throw new UnsupportedOperationException();
  }

  /**
   * A function that puts an element in a product-1.
   *
   * @return A function that puts an element in a product-1.
   */
  public static <A> F<A, P1<A>> p1() {
    return new F<A, P1<A>>() {
      public P1<A> f(final A a) {
        return p(a);
      }
    };
  }

  /**
   * A function that puts an element in a product-1.
   *
   * @param a The element.
   * @return The product-1.
   */
  public static <A> P1<A> p(final A a) {
    return new P1<A>() {
      public A _1() {
        return a;
      }
    };
  }

  /**
   * A function that puts an element in a product-2.
   *
   * @return A function that puts an element in a product-2.
   */
  public static <A, B> F<A, F<B, P2<A, B>>> p2() {
    return new F<A, F<B, P2<A, B>>>() {
      public F<B, P2<A, B>> f(final A a) {
        return new F<B, P2<A, B>>() {
          public P2<A, B> f(final B b) {
            return p(a, b);
          }
        };
      }
    };
  }

  /**
   * A function that puts elements in a product-2.
   *
   * @param a An element.
   * @param b An element.
   * @return The product-2.
   */
  public static <A, B> P2<A, B> p(final A a, final B b) {
    return new P2<A, B>() {
      public A _1() {
        return a;
      }

      public B _2() {
        return b;
      }
    };
  }

  /**
   * A function that puts an element in a product-3.
   *
   * @return A function that puts an element in a product-3.
   */
  public static <A, B, C> F<A, F<B, F<C, P3<A, B, C>>>> p3() {
    return new F<A, F<B, F<C, P3<A, B, C>>>>() {
      public F<B, F<C, P3<A, B, C>>> f(final A a) {
        return new F<B, F<C, P3<A, B, C>>>() {
          public F<C, P3<A, B, C>> f(final B b) {
            return new F<C, P3<A, B, C>>() {
              public P3<A, B, C> f(final C c) {
                return p(a, b, c);
              }
            };
          }
        };
      }
    };
  }

  /**
   * A function that puts elements in a product-3.
   *
   * @param a An element.
   * @param b An element.
   * @param c An element.
   * @return The product-3.
   */
  public static <A, B, C> P3<A, B, C> p(final A a, final B b, final C c) {
    return new P3<A, B, C>() {
      public A _1() {
        return a;
      }

      public B _2() {
        return b;
      }

      public C _3() {
        return c;
      }
    };
  }

  /**
   * A function that puts an element in a product-4.
   *
   * @return A function that puts an element in a product-4.
   */
  public static <A, B, C, D> F<A, F<B, F<C, F<D, P4<A, B, C, D>>>>> p4() {
    return new F<A, F<B, F<C, F<D, P4<A, B, C, D>>>>>() {
      public F<B, F<C, F<D, P4<A, B, C, D>>>> f(final A a) {
        return new F<B, F<C, F<D, P4<A, B, C, D>>>>() {
          public F<C, F<D, P4<A, B, C, D>>> f(final B b) {
            return new F<C, F<D, P4<A, B, C, D>>>() {
              public F<D, P4<A, B, C, D>> f(final C c) {
                return new F<D, P4<A, B, C, D>>() {
                  public P4<A, B, C, D> f(final D d) {
                    return p(a, b, c, d);
                  }
                };
              }
            };
          }
        };
      }
    };
  }

  /**
   * A function that puts elements in a product-4.
   *
   * @param a An element.
   * @param b An element.
   * @param c An element.
   * @param d An element.
   * @return The product-4.
   */
  public static <A, B, C, D> P4<A, B, C, D> p(final A a, final B b, final C c, final D d) {
    return new P4<A, B, C, D>() {
      public A _1() {
        return a;
      }

      public B _2() {
        return b;
      }

      public C _3() {
        return c;
      }

      public D _4() {
        return d;
      }
    };
  }

  /**
   * A function that puts an element in a product-5.
   *
   * @return A function that puts an element in a product-5.
   */
  public static <A, B, C, D, E> F<A, F<B, F<C, F<D, F<E, P5<A, B, C, D, E>>>>>> p5() {
    return new F<A, F<B, F<C, F<D, F<E, P5<A, B, C, D, E>>>>>>() {
      public F<B, F<C, F<D, F<E, P5<A, B, C, D, E>>>>> f(final A a) {
        return new F<B, F<C, F<D, F<E, P5<A, B, C, D, E>>>>>() {
          public F<C, F<D, F<E, P5<A, B, C, D, E>>>> f(final B b) {
            return new F<C, F<D, F<E, P5<A, B, C, D, E>>>>() {
              public F<D, F<E, P5<A, B, C, D, E>>> f(final C c) {
                return new F<D, F<E, P5<A, B, C, D, E>>>() {
                  public F<E, P5<A, B, C, D, E>> f(final D d) {
                    return new F<E, P5<A, B, C, D, E>>() {
                      public P5<A, B, C, D, E> f(final E e) {
                        return p(a, b, c, d, e);
                      }
                    };
                  }
                };
              }
            };
          }
        };
      }
    };
  }

  /**
   * A function that puts elements in a product-5.
   *
   * @param a An element.
   * @param b An element.
   * @param c An element.
   * @param d An element.
   * @param e An element.
   * @return The product-5.
   */
  public static <A, B, C, D, E> P5<A, B, C, D, E> p(final A a, final B b, final C c, final D d, final E e) {
    return new P5<A, B, C, D, E>() {
      public A _1() {
        return a;
      }

      public B _2() {
        return b;
      }

      public C _3() {
        return c;
      }

      public D _4() {
        return d;
      }

      public E _5() {
        return e;
      }
    };
  }

  /**
   * A function that puts an element in a product-6.
   *
   * @return A function that puts an element in a product-6.
   */
  public static <A, B, C, D, E, F$> F<A, F<B, F<C, F<D, F<E, F<F$, P6<A, B, C, D, E, F$>>>>>>> p6() {
    return new F<A, F<B, F<C, F<D, F<E, F<F$, P6<A, B, C, D, E, F$>>>>>>>() {
      public F<B, F<C, F<D, F<E, F<F$, P6<A, B, C, D, E, F$>>>>>> f(final A a) {
        return new F<B, F<C, F<D, F<E, F<F$, P6<A, B, C, D, E, F$>>>>>>() {
          public F<C, F<D, F<E, F<F$, P6<A, B, C, D, E, F$>>>>> f(final B b) {
            return new F<C, F<D, F<E, F<F$, P6<A, B, C, D, E, F$>>>>>() {
              public F<D, F<E, F<F$, P6<A, B, C, D, E, F$>>>> f(final C c) {
                return new F<D, F<E, F<F$, P6<A, B, C, D, E, F$>>>>() {
                  public F<E, F<F$, P6<A, B, C, D, E, F$>>> f(final D d) {
                    return new F<E, F<F$, P6<A, B, C, D, E, F$>>>() {
                      public F<F$, P6<A, B, C, D, E, F$>> f(final E e) {
                        return new F<F$, P6<A, B, C, D, E, F$>>() {
                          public P6<A, B, C, D, E, F$> f(final F$ f) {
                            return p(a, b, c, d, e, f);
                          }
                        };
                      }
                    };
                  }
                };
              }
            };
          }
        };
      }
    };
  }

  /**
   * A function that puts elements in a product-6.
   *
   * @param a An element.
   * @param b An element.
   * @param c An element.
   * @param d An element.
   * @param e An element.
   * @param f An element.
   * @return The product-6.
   */
  public static <A, B, C, D, E, F$> P6<A, B, C, D, E, F$> p(final A a, final B b, final C c, final D d, final E e, final F$ f) {
    return new P6<A, B, C, D, E, F$>() {
      public A _1() {
        return a;
      }

      public B _2() {
        return b;
      }

      public C _3() {
        return c;
      }

      public D _4() {
        return d;
      }

      public E _5() {
        return e;
      }

      public F$ _6() {
        return f;
      }
    };
  }

  /**
   * A function that puts an element in a product-7.
   *
   * @return A function that puts an element in a product-7.
   */
  public static <A, B, C, D, E, F$, G> F<A, F<B, F<C, F<D, F<E, F<F$, F<G, P7<A, B, C, D, E, F$, G>>>>>>>> p7() {
    return new F<A, F<B, F<C, F<D, F<E, F<F$, F<G, P7<A, B, C, D, E, F$, G>>>>>>>>() {
      public F<B, F<C, F<D, F<E, F<F$, F<G, P7<A, B, C, D, E, F$, G>>>>>>> f(final A a) {
        return new F<B, F<C, F<D, F<E, F<F$, F<G, P7<A, B, C, D, E, F$, G>>>>>>>() {
          public F<C, F<D, F<E, F<F$, F<G, P7<A, B, C, D, E, F$, G>>>>>> f(final B b) {
            return new F<C, F<D, F<E, F<F$, F<G, P7<A, B, C, D, E, F$, G>>>>>>() {
              public F<D, F<E, F<F$, F<G, P7<A, B, C, D, E, F$, G>>>>> f(final C c) {
                return new F<D, F<E, F<F$, F<G, P7<A, B, C, D, E, F$, G>>>>>() {
                  public F<E, F<F$, F<G, P7<A, B, C, D, E, F$, G>>>> f(final D d) {
                    return new F<E, F<F$, F<G, P7<A, B, C, D, E, F$, G>>>>() {
                      public F<F$, F<G, P7<A, B, C, D, E, F$, G>>> f(final E e) {
                        return new F<F$, F<G, P7<A, B, C, D, E, F$, G>>>() {
                          public F<G, P7<A, B, C, D, E, F$, G>> f(final F$ f) {
                            return new F<G, P7<A, B, C, D, E, F$, G>>() {
                              public P7<A, B, C, D, E, F$, G> f(final G g) {
                                return p(a, b, c, d, e, f, g);
                              }
                            };
                          }
                        };
                      }
                    };
                  }
                };
              }
            };
          }
        };
      }
    };
  }
  
  /**
   * A function that puts elements in a product-7.
   *
   * @param a An element.
   * @param b An element.
   * @param c An element.
   * @param d An element.
   * @param e An element.
   * @param f An element.
   * @param g An element.
   * @return The product-7.
   */
  public static <A, B, C, D, E, F$, G> P7<A, B, C, D, E, F$, G> p(final A a, final B b, final C c, final D d, final E e, final F$ f, final G g) {
    return new P7<A, B, C, D, E, F$, G>() {
      public A _1() {
        return a;
      }

      public B _2() {
        return b;
      }

      public C _3() {
        return c;
      }

      public D _4() {
        return d;
      }

      public E _5() {
        return e;
      }

      public F$ _6() {
        return f;
      }

      public G _7() {
        return g;
      }
    };
  }

  /**
   * A function that puts an element in a product-8.
   *
   * @return A function that puts an element in a product-8.
   */
  public static <A, B, C, D, E, F$, G, H> F<A, F<B, F<C, F<D, F<E, F<F$, F<G, F<H, P8<A, B, C, D, E, F$, G, H>>>>>>>>> p8() {
    return new F<A, F<B, F<C, F<D, F<E, F<F$, F<G, F<H, P8<A, B, C, D, E, F$, G, H>>>>>>>>>() {
      public F<B, F<C, F<D, F<E, F<F$, F<G, F<H, P8<A, B, C, D, E, F$, G, H>>>>>>>> f(final A a) {
        return new F<B, F<C, F<D, F<E, F<F$, F<G, F<H, P8<A, B, C, D, E, F$, G, H>>>>>>>>() {
          public F<C, F<D, F<E, F<F$, F<G, F<H, P8<A, B, C, D, E, F$, G, H>>>>>>> f(final B b) {
            return new F<C, F<D, F<E, F<F$, F<G, F<H, P8<A, B, C, D, E, F$, G, H>>>>>>>() {
              public F<D, F<E, F<F$, F<G, F<H, P8<A, B, C, D, E, F$, G, H>>>>>> f(final C c) {
                return new F<D, F<E, F<F$, F<G, F<H, P8<A, B, C, D, E, F$, G, H>>>>>>() {
                  public F<E, F<F$, F<G, F<H, P8<A, B, C, D, E, F$, G, H>>>>> f(final D d) {
                    return new F<E, F<F$, F<G, F<H, P8<A, B, C, D, E, F$, G, H>>>>>() {
                      public F<F$, F<G, F<H, P8<A, B, C, D, E, F$, G, H>>>> f(final E e) {
                        return new F<F$, F<G, F<H, P8<A, B, C, D, E, F$, G, H>>>>() {
                          public F<G, F<H, P8<A, B, C, D, E, F$, G, H>>> f(final F$ f) {
                            return new F<G, F<H, P8<A, B, C, D, E, F$, G, H>>>() {
                              public F<H, P8<A, B, C, D, E, F$, G, H>> f(final G g) {
                                return new F<H, P8<A, B, C, D, E, F$, G, H>>() {
                                  public P8<A, B, C, D, E, F$, G, H> f(final H h) {
                                    return p(a, b, c, d, e, f, g, h);
                                  }
                                };
                              }
                            };
                          }
                        };
                      }
                    };
                  }
                };
              }
            };
          }
        };
      }
    };
  }

  /**
   * A function that puts elements in a product-8.
   *
   * @param a An element.
   * @param b An element.
   * @param c An element.
   * @param d An element.
   * @param e An element.
   * @param f An element.
   * @param g An element.
   * @param h An element.
   * @return The product-8.
   */
  public static <A, B, C, D, E, F$, G, H> P8<A, B, C, D, E, F$, G, H> p(final A a, final B b, final C c, final D d, final E e, final F$ f, final G g, final H h) {
    return new P8<A, B, C, D, E, F$, G, H>() {
      public A _1() {
        return a;
      }

      public B _2() {
        return b;
      }

      public C _3() {
        return c;
      }

      public D _4() {
        return d;
      }

      public E _5() {
        return e;
      }

      public F$ _6() {
        return f;
      }

      public G _7() {
        return g;
      }

      public H _8() {
        return h;
      }
    };
  }
}
