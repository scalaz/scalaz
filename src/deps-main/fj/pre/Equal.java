package fj.pre;

import fj.F;
import fj.F2;
import fj.Function;
import fj.P1;
import fj.P2;
import fj.P3;
import fj.P4;
import fj.P5;
import fj.P6;
import fj.P7;
import fj.P8;
import static fj.Function.compose;
import static fj.Function.curry;
import fj.data.Array;
import fj.data.Either;
import fj.data.hlist.HList;
import fj.data.List;
import fj.data.NonEmptyList;
import fj.data.Option;
import fj.data.Set;
import fj.data.Stream;
import fj.data.Tree;
import fj.data.Validation;
import fj.data.vector.V2;
import fj.data.vector.V3;
import fj.data.vector.V4;
import fj.data.vector.V5;
import fj.data.vector.V6;
import fj.data.vector.V7;
import fj.data.vector.V8;

import java.math.BigInteger;
import java.math.BigDecimal;

/**
 * Tests for equality between two objects.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class Equal<A> {
  private final F<A, F<A, Boolean>> f;

  private Equal(final F<A, F<A, Boolean>> f) {
    this.f = f;
  }

  /**
   * Returns <code>true</code> if the two given arguments are equal, <code>false</code> otherwise.
   *
   * @param a1 An object to test for equality against another.
   * @param a2 An object to test for equality against another.
   * @return <code>true</code> if the two given arguments are equal, <code>false</code> otherwise.
   */
  public boolean eq(final A a1, final A a2) {
    return f.f(a1).f(a2);
  }

  /**
   * First-class equality check.
   *
   * @return A function that returns <code>true</code> if the two given arguments are equal.
   */
  public F2<A, A, Boolean> eq() {
    return new F2<A, A, Boolean>() {
      public Boolean f(final A a, final A a1) {
        return eq(a, a1);
      }
    };
  }

  /**
   * Partially applied equality check.
   *
   * @param a An object to test for equality against another.
   * @return A function that returns <code>true</code> if the given argument equals the argument to this method.
   */
  public F<A, Boolean> eq(final A a) {
    return new F<A, Boolean>() {
      public Boolean f(final A a1) {
        return eq(a, a1);
      }
    };
  }

  /**
   * Maps the given function across this equal as a contra-variant functor.
   *
   * @param f The function to map.
   * @return A new equal.
   */
  public <B> Equal<B> comap(final F<B, A> f) {
    return equal(compose(compose(Function.<B, A, Boolean>andThen().f(f), this.f), f));
  }

  /**
   * Constructs an equal instance from the given function.
   *
   * @param f The function to construct the equal with.
   * @return An equal instance from the given function.
   */
  public static <A> Equal<A> equal(final F<A, F<A, Boolean>> f) {
    return new Equal<A>(f);
  }

  /**
   * Returns an equal instance that uses the {@link Object#equals(Object)} method to test for
   * equality.
   *
   * @return An equal instance that uses the {@link Object#equals(Object)} method to test for
   *         equality.
   */
  public static <A> Equal<A> anyEqual() {
    return new Equal<A>(new F<A, F<A, Boolean>>() {
      public F<A, Boolean> f(final A a1) {
        return new F<A, Boolean>() {
          public Boolean f(final A a2) {
            return a1.equals(a2);
          }
        };
      }
    });
  }

  /**
   * An equal instance for the <code>boolean</code> type.
   */
  public static final Equal<Boolean> booleanEqual = anyEqual();

  /**
   * An equal instance for the <code>byte</code> type.
   */
  public static final Equal<Byte> byteEqual = anyEqual();

  /**
   * An equal instance for the <code>char</code> type.
   */
  public static final Equal<Character> charEqual = anyEqual();

  /**
   * An equal instance for the <code>double</code> type.
   */
  public static final Equal<Double> doubleEqual = anyEqual();

  /**
   * An equal instance for the <code>float</code> type.
   */
  public static final Equal<Float> floatEqual = anyEqual();

  /**
   * An equal instance for the <code>int</code> type.
   */
  public static final Equal<Integer> intEqual = anyEqual();

  /**
   * An equal instance for the <code>BigInteger</code> type.
   */
  public static final Equal<BigInteger> bigintEqual = anyEqual();

  /**
   * An equal instance for the <code>BigDecimal</code> type.
   */
  public static final Equal<BigDecimal> bigdecimalEqual = anyEqual();

  /**
   * An equal instance for the <code>long</code> type.
   */
  public static final Equal<Long> longEqual = anyEqual();

  /**
   * An equal instance for the <code>short</code> type.
   */
  public static final Equal<Short> shortEqual = anyEqual();

  /**
   * An equal instance for the {@link String} type.
   */
  public static final Equal<String> stringEqual = anyEqual();

  /**
   * An equal instance for the {@link StringBuffer} type.
   */
  public static final Equal<StringBuffer> stringBufferEqual = new Equal<StringBuffer>(new F<StringBuffer, F<StringBuffer, Boolean>>() {
    public F<StringBuffer, Boolean> f(final StringBuffer sb1) {
      return new F<StringBuffer, Boolean>() {
        public Boolean f(final StringBuffer sb2) {
          if (sb1.length() == sb2.length()) {
            for (int i = 0; i < sb1.length(); i++)
              if (sb1.charAt(i) != sb2.charAt(i))
                return false;
            return true;
          } else
            return false;
        }
      };
    }
  });

  /**
   * An equal instance for the {@link StringBuilder} type.
   */
  public static final Equal<StringBuilder> stringBuilderEqual = new Equal<StringBuilder>(new F<StringBuilder, F<StringBuilder, Boolean>>() {
    public F<StringBuilder, Boolean> f(final StringBuilder sb1) {
      return new F<StringBuilder, Boolean>() {
        public Boolean f(final StringBuilder sb2) {
          if (sb1.length() == sb2.length()) {
            for (int i = 0; i < sb1.length(); i++)
              if (sb1.charAt(i) != sb2.charAt(i))
                return false;
            return true;
          } else
            return false;
        }
      };
    }
  });

  /**
   * An equal instance for the {@link Either} type.
   *
   * @param ea Equality across the left side of {@link Either}.
   * @param eb Equality across the right side of {@link Either}.
   * @return An equal instance for the {@link Either} type.
   */
  public static <A, B> Equal<Either<A, B>> eitherEqual(final Equal<A> ea, final Equal<B> eb) {
    return new Equal<Either<A, B>>(new F<Either<A, B>, F<Either<A, B>, Boolean>>() {
      public F<Either<A, B>, Boolean> f(final Either<A, B> e1) {
        return new F<Either<A, B>, Boolean>() {
          public Boolean f(final Either<A, B> e2) {
            return e1.isLeft() && e2.isLeft() && ea.f.f(e1.left().value()).f(e2.left().value()) ||
              e1.isRight() && e2.isRight() && eb.f.f(e1.right().value()).f(e2.right().value());
          }
        };
      }
    });
  }

  /**
   * An equal instance for the {@link Validation} type.
   *
   * @param ea Equality across the failing side of {@link Validation}.
   * @param eb Equality across the succeeding side of {@link Validation}.
   * @return An equal instance for the {@link Validation} type.
   */
  public static <A, B> Equal<Validation<A, B>> validationEqual(final Equal<A> ea, final Equal<B> eb) {
    return eitherEqual(ea, eb).comap(Validation.<A, B>either());
  }

  /**
   * An equal instance for the {@link List} type.
   *
   * @param ea Equality across the elements of the list.
   * @return An equal instance for the {@link List} type.
   */
  public static <A> Equal<List<A>> listEqual(final Equal<A> ea) {
    return new Equal<List<A>>(new F<List<A>, F<List<A>, Boolean>>() {
      public F<List<A>, Boolean> f(final List<A> a1) {
        return new F<List<A>, Boolean>() {
          public Boolean f(final List<A> a2) {
            List<A> x1 = a1;
            List<A> x2 = a2;

            while (x1.isNotEmpty() && x2.isNotEmpty()) {
              if (!ea.eq(x1.head(), x2.head()))
                return false;

              x1 = x1.tail();
              x2 = x2.tail();
            }

            return x1.isEmpty() && x2.isEmpty();
          }
        };
      }
    });
  }

  /**
   * An equal instance for the {@link NonEmptyList} type.
   *
   * @param ea Equality across the elements of the non-empty list.
   * @return An equal instance for the {@link NonEmptyList} type.
   */
  public static <A> Equal<NonEmptyList<A>> nonEmptyListEqual(final Equal<A> ea) {
    return listEqual(ea).comap(NonEmptyList.<A>toList_());
  }

  /**
   * An equal instance for the {@link Option} type.
   *
   * @param ea Equality across the element of the option.
   * @return An equal instance for the {@link Option} type.
   */
  public static <A> Equal<Option<A>> optionEqual(final Equal<A> ea) {
    return new Equal<Option<A>>(new F<Option<A>, F<Option<A>, Boolean>>() {
      public F<Option<A>, Boolean> f(final Option<A> o1) {
        return new F<Option<A>, Boolean>() {
          public Boolean f(final Option<A> o2) {
            return o1.isNone() && o2.isNone() ||
              o1.isSome() && o2.isSome() && ea.f.f(o1.some()).f(o2.some());
          }
        };
      }
    });
  }

  /**
   * An equal instance for the {@link Stream} type.
   *
   * @param ea Equality across the elements of the stream.
   * @return An equal instance for the {@link Stream} type.
   */
  public static <A> Equal<Stream<A>> streamEqual(final Equal<A> ea) {
    return new Equal<Stream<A>>(new F<Stream<A>, F<Stream<A>, Boolean>>() {
      public F<Stream<A>, Boolean> f(final Stream<A> a1) {
        return new F<Stream<A>, Boolean>() {
          public Boolean f(final Stream<A> a2) {
            Stream<A> x1 = a1;
            Stream<A> x2 = a2;

            while (x1.isNotEmpty() && x2.isNotEmpty()) {
              if (!ea.eq(x1.head(), x2.head()))
                return false;

              x1 = x1.tail()._1();
              x2 = x2.tail()._1();
            }

            return x1.isEmpty() && x2.isEmpty();
          }
        };
      }
    });
  }

  /**
   * An equal instance for the {@link Array} type.
   *
   * @param ea Equality across the elements of the array.
   * @return An equal instance for the {@link Array} type.
   */
  public static <A> Equal<Array<A>> arrayEqual(final Equal<A> ea) {
    return new Equal<Array<A>>(new F<Array<A>, F<Array<A>, Boolean>>() {
      public F<Array<A>, Boolean> f(final Array<A> a1) {
        return new F<Array<A>, Boolean>() {
          public Boolean f(final Array<A> a2) {
            if (a1.length() == a2.length()) {
              for (int i = 0; i < a1.length(); i++) {
                if (!ea.eq(a1.get(i), a2.get(i)))
                  return false;
              }
              return true;
            } else
              return false;
          }
        };
      }
    });
  }

  /**
   * An equal instance for the {@link Tree} type.
   *
   * @param ea Equality across the elements of the tree.
   * @return An equal instance for the {@link Tree} type.
   */
  public static <A> Equal<Tree<A>> treeEqual(final Equal<A> ea) {
    return new Equal<Tree<A>>(curry(new F2<Tree<A>, Tree<A>, Boolean>() {
      public Boolean f(final Tree<A> t1, final Tree<A> t2) {
        return ea.eq(t1.root(), t2.root()) && listEqual(treeEqual(ea)).eq(t2.subForest(), t1.subForest());
      }
    }));
  }

  /**
   * An equal instance for a product-1.
   *
   * @param ea Equality across the first element of the product.
   * @return An equal instance for a product-1.
   */
  public static <A> Equal<P1<A>> p1Equal(final Equal<A> ea) {
    return new Equal<P1<A>>(new F<P1<A>, F<P1<A>, Boolean>>() {
      public F<P1<A>, Boolean> f(final P1<A> p1) {
        return new F<P1<A>, Boolean>() {
          public Boolean f(final P1<A> p2) {
            return ea.eq(p1._1(), p2._1());
          }
        };
      }
    });
  }

  /**
   * An equal instance for a product-2.
   *
   * @param ea Equality across the first element of the product.
   * @param eb Equality across the second element of the product.
   * @return An equal instance for a product-2.
   */
  public static <A, B> Equal<P2<A, B>> p2Equal(final Equal<A> ea, final Equal<B> eb) {
    return new Equal<P2<A, B>>(new F<P2<A, B>, F<P2<A, B>, Boolean>>() {
      public F<P2<A, B>, Boolean> f(final P2<A, B> p1) {
        return new F<P2<A, B>, Boolean>() {
          public Boolean f(final P2<A, B> p2) {
            return ea.eq(p1._1(), p2._1()) && eb.eq(p1._2(), p2._2());
          }
        };
      }
    });
  }

  /**
   * An equal instance for a product-3.
   *
   * @param ea Equality across the first element of the product.
   * @param eb Equality across the second element of the product.
   * @param ec Equality across the third element of the product.
   * @return An equal instance for a product-3.
   */
  public static <A, B, C> Equal<P3<A, B, C>> p3Equal(final Equal<A> ea, final Equal<B> eb, final Equal<C> ec) {
    return new Equal<P3<A, B, C>>(new F<P3<A, B, C>, F<P3<A, B, C>, Boolean>>() {
      public F<P3<A, B, C>, Boolean> f(final P3<A, B, C> p1) {
        return new F<P3<A, B, C>, Boolean>() {
          public Boolean f(final P3<A, B, C> p2) {
            return ea.eq(p1._1(), p2._1()) && eb.eq(p1._2(), p2._2()) && ec.eq(p1._3(), p2._3());
          }
        };
      }
    });
  }

  /**
   * An equal instance for a product-4.
   *
   * @param ea Equality across the first element of the product.
   * @param eb Equality across the second element of the product.
   * @param ec Equality across the third element of the product.
   * @param ed Equality across the fourth element of the product.
   * @return An equal instance for a product-4.
   */
  public static <A, B, C, D> Equal<P4<A, B, C, D>> p4Equal(final Equal<A> ea, final Equal<B> eb, final Equal<C> ec, final Equal<D> ed) {
    return new Equal<P4<A, B, C, D>>(new F<P4<A, B, C, D>, F<P4<A, B, C, D>, Boolean>>() {
      public F<P4<A, B, C, D>, Boolean> f(final P4<A, B, C, D> p1) {
        return new F<P4<A, B, C, D>, Boolean>() {
          public Boolean f(final P4<A, B, C, D> p2) {
            return ea.eq(p1._1(), p2._1()) && eb.eq(p1._2(), p2._2()) && ec.eq(p1._3(), p2._3()) &&
              ed.eq(p1._4(), p2._4());
          }
        };
      }
    });
  }

  /**
   * An equal instance for a product-5.
   *
   * @param ea Equality across the first element of the product.
   * @param eb Equality across the second element of the product.
   * @param ec Equality across the third element of the product.
   * @param ed Equality across the fourth element of the product.
   * @param ee Equality across the fifth element of the product.
   * @return An equal instance for a product-5.
   */
  public static <A, B, C, D, E> Equal<P5<A, B, C, D, E>> p5Equal(final Equal<A> ea, final Equal<B> eb, final Equal<C> ec, final Equal<D> ed, final Equal<E> ee) {
    return new Equal<P5<A, B, C, D, E>>(new F<P5<A, B, C, D, E>, F<P5<A, B, C, D, E>, Boolean>>() {
      public F<P5<A, B, C, D, E>, Boolean> f(final P5<A, B, C, D, E> p1) {
        return new F<P5<A, B, C, D, E>, Boolean>() {
          public Boolean f(final P5<A, B, C, D, E> p2) {
            return ea.eq(p1._1(), p2._1()) && eb.eq(p1._2(), p2._2()) && ec.eq(p1._3(), p2._3()) &&
              ed.eq(p1._4(), p2._4()) && ee.eq(p1._5(), p2._5());
          }
        };
      }
    });
  }

  /**
   * An equal instance for a product-6.
   *
   * @param ea Equality across the first element of the product.
   * @param eb Equality across the second element of the product.
   * @param ec Equality across the third element of the product.
   * @param ed Equality across the fourth element of the product.
   * @param ee Equality across the fifth element of the product.
   * @param ef Equality across the sixth element of the product.
   * @return An equal instance for a product-6.
   */
  public static <A, B, C, D, E, F$> Equal<P6<A, B, C, D, E, F$>> p6Equal(final Equal<A> ea, final Equal<B> eb, final Equal<C> ec, final Equal<D> ed, final Equal<E> ee, final Equal<F$> ef) {
    return new Equal<P6<A, B, C, D, E, F$>>(new F<P6<A, B, C, D, E, F$>, F<P6<A, B, C, D, E, F$>, Boolean>>() {
      public F<P6<A, B, C, D, E, F$>, Boolean> f(final P6<A, B, C, D, E, F$> p1) {
        return new F<P6<A, B, C, D, E, F$>, Boolean>() {
          public Boolean f(final P6<A, B, C, D, E, F$> p2) {
            return ea.eq(p1._1(), p2._1()) && eb.eq(p1._2(), p2._2()) && ec.eq(p1._3(), p2._3()) &&
              ed.eq(p1._4(), p2._4()) && ee.eq(p1._5(), p2._5()) && ef.eq(p1._6(), p2._6());
          }
        };
      }
    });
  }

  /**
   * An equal instance for a product-7.
   *
   * @param ea Equality across the first element of the product.
   * @param eb Equality across the second element of the product.
   * @param ec Equality across the third element of the product.
   * @param ed Equality across the fourth element of the product.
   * @param ee Equality across the fifth element of the product.
   * @param ef Equality across the sixth element of the product.
   * @param eg Equality across the seventh element of the product.
   * @return An equal instance for a product-7.
   */
  public static <A, B, C, D, E, F$, G> Equal<P7<A, B, C, D, E, F$, G>> p7Equal(final Equal<A> ea, final Equal<B> eb, final Equal<C> ec, final Equal<D> ed, final Equal<E> ee, final Equal<F$> ef, final Equal<G> eg) {
    return new Equal<P7<A, B, C, D, E, F$, G>>(new F<P7<A, B, C, D, E, F$, G>, F<P7<A, B, C, D, E, F$, G>, Boolean>>() {
      public F<P7<A, B, C, D, E, F$, G>, Boolean> f(final P7<A, B, C, D, E, F$, G> p1) {
        return new F<P7<A, B, C, D, E, F$, G>, Boolean>() {
          public Boolean f(final P7<A, B, C, D, E, F$, G> p2) {
            return ea.eq(p1._1(), p2._1()) && eb.eq(p1._2(), p2._2()) && ec.eq(p1._3(), p2._3()) &&
              ed.eq(p1._4(), p2._4()) && ee.eq(p1._5(), p2._5()) && ef.eq(p1._6(), p2._6()) &&
              eg.eq(p1._7(), p2._7());
          }
        };
      }
    });
  }

  /**
   * An equal instance for a product-8.
   *
   * @param ea Equality across the first element of the product.
   * @param eb Equality across the second element of the product.
   * @param ec Equality across the third element of the product.
   * @param ed Equality across the fourth element of the product.
   * @param ee Equality across the fifth element of the product.
   * @param ef Equality across the sixth element of the product.
   * @param eg Equality across the seventh element of the product.
   * @param eh Equality across the eighth element of the product.
   * @return An equal instance for a product-8.
   */
  public static <A, B, C, D, E, F$, G, H> Equal<P8<A, B, C, D, E, F$, G, H>> p8Equal(final Equal<A> ea, final Equal<B> eb, final Equal<C> ec, final Equal<D> ed, final Equal<E> ee, final Equal<F$> ef, final Equal<G> eg, final Equal<H> eh) {
    return new Equal<P8<A, B, C, D, E, F$, G, H>>(new F<P8<A, B, C, D, E, F$, G, H>, F<P8<A, B, C, D, E, F$, G, H>, Boolean>>() {
      public F<P8<A, B, C, D, E, F$, G, H>, Boolean> f(final P8<A, B, C, D, E, F$, G, H> p1) {
        return new F<P8<A, B, C, D, E, F$, G, H>, Boolean>() {
          public Boolean f(final P8<A, B, C, D, E, F$, G, H> p2) {
            return ea.eq(p1._1(), p2._1()) && eb.eq(p1._2(), p2._2()) && ec.eq(p1._3(), p2._3()) &&
              ed.eq(p1._4(), p2._4()) && ee.eq(p1._5(), p2._5()) && ef.eq(p1._6(), p2._6()) &&
              eg.eq(p1._7(), p2._7()) && eh.eq(p1._8(), p2._8());
          }
        };
      }
    });
  }

  /**
   * An equal instance for a vector-2.
   *
   * @param ea Equality across the elements of the vector.
   * @return An equal instance for a vector-2.
   */
  public static <A> Equal<V2<A>> v2Equal(final Equal<A> ea) {
    return streamEqual(ea).comap(V2.<A>toStream_());
  }

  /**
   * An equal instance for a vector-3.
   *
   * @param ea Equality across the elements of the vector.
   * @return An equal instance for a vector-3.
   */
  public static <A> Equal<V3<A>> v3Equal(final Equal<A> ea) {
    return streamEqual(ea).comap(V3.<A>toStream_());
  }

  /**
   * An equal instance for a vector-4.
   *
   * @param ea Equality across the elements of the vector.
   * @return An equal instance for a vector-4.
   */
  public static <A> Equal<V4<A>> v4Equal(final Equal<A> ea) {
    return streamEqual(ea).comap(V4.<A>toStream_());
  }

  /**
   * An equal instance for a vector-5.
   *
   * @param ea Equality across the elements of the vector.
   * @return An equal instance for a vector-5.
   */
  public static <A> Equal<V5<A>> v5Equal(final Equal<A> ea) {
    return streamEqual(ea).comap(V5.<A>toStream_());
  }

  /**
   * An equal instance for a vector-6.
   *
   * @param ea Equality across the elements of the vector.
   * @return An equal instance for a vector-6.
   */
  public static <A> Equal<V6<A>> v6Equal(final Equal<A> ea) {
    return streamEqual(ea).comap(V6.<A>toStream_());
  }

  /**
   * An equal instance for a vector-7.
   *
   * @param ea Equality across the elements of the vector.
   * @return An equal instance for a vector-7.
   */
  public static <A> Equal<V7<A>> v7Equal(final Equal<A> ea) {
    return streamEqual(ea).comap(V7.<A>toStream_());
  }

  /**
   * An equal instance for a vector-8.
   *
   * @param ea Equality across the elements of the vector.
   * @return An equal instance for a vector-8.
   */
  public static <A> Equal<V8<A>> v8Equal(final Equal<A> ea) {
    return streamEqual(ea).comap(V8.<A>toStream_());
  }

  /**
   * An equal instance for the empty heterogeneous list.
   */
  public static final Equal<HList.HNil> hListEqual = anyEqual();

  /**
   * An equal instance for heterogeneous lists.
   *
   * @param e Equality for the first element of the list.
   * @param l Equality for the rest of the list.
   * @return an equal instance for a heterogeneous list.
   */
  public static <E, L extends HList<L>> Equal<HList.HCons<E, L>> hListEqual(final Equal<E> e, final Equal<L> l) {
    return equal(curry(new F2<HList.HCons<E, L>, HList.HCons<E, L>, Boolean>() {
      public Boolean f(final HList.HCons<E, L> c1, final HList.HCons<E, L> c2) {
        return e.eq(c1.head(), c2.head()) && l.eq(c1.tail(), c2.tail());
      }
    }));
  }

  /**
   * Equal instance for sets.
   *
   * @param e Equality for the set elements.
   * @return An equal instance for sets.
   */
  public static <A> Equal<Set<A>> setEqual(final Equal<A> e) {
    return equal(curry(new F2<Set<A>, Set<A>, Boolean>() {
      public Boolean f(final Set<A> a, final Set<A> b) {
        return streamEqual(e).eq(a.toStream(), b.toStream());
      }
    }));
  }
}
