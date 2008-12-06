package fj.pre;

import fj.F;
import fj.F2;
import fj.Function;
import static fj.Function.compose;
import static fj.Function.curry;
import fj.data.Array;
import fj.data.Either;
import fj.data.List;
import fj.data.NonEmptyList;
import fj.data.Option;
import fj.data.Set;
import fj.data.Stream;
import fj.data.Validation;
import static fj.pre.Ordering.EQ;
import static fj.pre.Ordering.GT;
import static fj.pre.Ordering.LT;

import java.math.BigInteger;
import java.math.BigDecimal;

/**
 * Tests for ordering between two objects.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class Ord<A> {
  private final F<A, F<A, Ordering>> f;

  private Ord(final F<A, F<A, Ordering>> f) {
    this.f = f;
  }

  /**
   * Returns an ordering for the given arguments.
   *
   * @param a1 An instance to compare for ordering to another.
   * @param a2 An instance to compare for ordering to another.
   * @return An ordering for the given arguments.
   */
  public Ordering compare(final A a1, final A a2) {
    return f.f(a1).f(a2);
  }

  /**
   * Returns <code>true</code> if the given arguments are equal, <code>false</code> otherwise.
   *
   * @param a1 An instance to compare for equality to another.
   * @param a2 An instance to compare for equality to another.
   * @return <code>true</code> if the given arguments are equal, <code>false</code> otherwise.
   */
  public boolean eq(final A a1, final A a2) {
    return compare(a1, a2) == EQ;
  }

  /**
   * Returns an <code>Equal</code> for this order.
   *
   * @return An <code>Equal</code> for this order.
   */
  public Equal<A> equal() {
    return Equal.equal(curry(new F2<A, A, Boolean>() {
      public Boolean f(final A a1, final A a2) {
        return eq(a1, a2);
      }
    }));
  }

  /**
   * Maps the given function across this ord as a contra-variant functor.
   *
   * @param f The function to map.
   * @return A new ord.
   */
  public <B> Ord<B> comap(final F<B, A> f) {
    return ord(compose(compose(Function.<B, A, Ordering>andThen().f(f), this.f), f));
  }

  /**
   * Returns <code>true</code> if the first given argument is less than the second given argument,
   * <code>false</code> otherwise.
   *
   * @param a1 An instance to compare for ordering to another.
   * @param a2 An instance to compare for ordering to another.
   * @return <code>true</code> if the first given argument is less than the second given argument,
   *         <code>false</code> otherwise.
   */
  public boolean isLessThan(final A a1, final A a2) {
    return compare(a1, a2) == LT;
  }

  /**
   * Returns <code>true</code> if the first given argument is greater than the second given
   * argument, <code>false</code> otherwise.
   *
   * @param a1 An instance to compare for ordering to another.
   * @param a2 An instance to compare for ordering to another.
   * @return <code>true</code> if the first given argument is greater than the second given
   *         argument, <code>false</code> otherwise.
   */
  public boolean isGreaterThan(final A a1, final A a2) {
    return compare(a1, a2) == GT;
  }

  /**
   * Returns a function that returns true if its argument is less than the argument to this method.
   *
   * @param a A value to compare against.
   * @return A function that returns true if its argument is less than the argument to this method.
   */
  public F<A, Boolean> isLessThan(final A a) {
    return new F<A, Boolean>() {
      public Boolean f(final A a2) {
        return compare(a2, a) == LT;
      }
    };
  }

  /**
   * Returns an order instance that uses the given equality test and ordering function.
   *
   * @param f The order function.
   * @return An order instance.
   */
  public static <A> Ord<A> ord(final F<A, F<A, Ordering>> f) {
    return new Ord<A>(f);
  }

  /**
   * An order instance for the <code>boolean</code> type.
   */
  public static final Ord<Boolean> booleanOrd = new Ord<Boolean>(
    new F<Boolean, F<Boolean, Ordering>>() {
      public F<Boolean, Ordering> f(final Boolean a1) {
        return new F<Boolean, Ordering>() {
          public Ordering f(final Boolean a2) {
            final int x = a1.compareTo(a2);
            return x < 0 ? LT : x == 0 ? EQ : GT;
          }
        };
      }
    });

  /**
   * An order instance for the <code>byte</code> type.
   */
  public static final Ord<Byte> byteOrd = new Ord<Byte>(
    new F<Byte, F<Byte, Ordering>>() {
      public F<Byte, Ordering> f(final Byte a1) {
        return new F<Byte, Ordering>() {
          public Ordering f(final Byte a2) {
            final int x = a1.compareTo(a2);
            return x < 0 ? LT : x == 0 ? EQ : GT;
          }
        };
      }
    });

  /**
   * An order instance for the <code>char</code> type.
   */
  public static final Ord<Character> charOrd = new Ord<Character>(
    new F<Character, F<Character, Ordering>>() {
      public F<Character, Ordering> f(final Character a1) {
        return new F<Character, Ordering>() {
          public Ordering f(final Character a2) {
            final int x = a1.compareTo(a2);
            return x < 0 ? LT : x == 0 ? EQ : GT;
          }
        };
      }
    });

  /**
   * An order instance for the <code>double</code> type.
   */
  public static final Ord<Double> doubleOrd = new Ord<Double>(
    new F<Double, F<Double, Ordering>>() {
      public F<Double, Ordering> f(final Double a1) {
        return new F<Double, Ordering>() {
          public Ordering f(final Double a2) {
            final int x = a1.compareTo(a2);
            return x < 0 ? LT : x == 0 ? EQ : GT;
          }
        };
      }
    });

  /**
   * An order instance for the <code>float</code> type.
   */
  public static final Ord<Float> floatOrd = new Ord<Float>(
    new F<Float, F<Float, Ordering>>() {
      public F<Float, Ordering> f(final Float a1) {
        return new F<Float, Ordering>() {
          public Ordering f(final Float a2) {
            final int x = a1.compareTo(a2);
            return x < 0 ? LT : x == 0 ? EQ : GT;
          }
        };
      }
    });

  /**
   * An order instance for the <code>int</code> type.
   */
  public static final Ord<Integer> intOrd = new Ord<Integer>(
    new F<Integer, F<Integer, Ordering>>() {
      public F<Integer, Ordering> f(final Integer a1) {
        return new F<Integer, Ordering>() {
          public Ordering f(final Integer a2) {
            final int x = a1.compareTo(a2);
            return x < 0 ? LT : x == 0 ? EQ : GT;
          }
        };
      }
    });

  /**
   * An order instance for the <code>BigInteger</code> type.
   */
  public static final Ord<BigInteger> bigintOrd = new Ord<BigInteger>(
    new F<BigInteger, F<BigInteger, Ordering>>() {
      public F<BigInteger, Ordering> f(final BigInteger a1) {
        return new F<BigInteger, Ordering>() {
          public Ordering f(final BigInteger a2) {
            final int x = a1.compareTo(a2);
            return x < 0 ? LT : x == 0 ? EQ : GT;
          }
        };
      }
    });

  /**
   * An order instance for the <code>BigDecimal</code> type.
   */
  public static final Ord<BigDecimal> bigdecimalOrd = new Ord<BigDecimal>(
    new F<BigDecimal, F<BigDecimal, Ordering>>() {
      public F<BigDecimal, Ordering> f(final BigDecimal a1) {
        return new F<BigDecimal, Ordering>() {
          public Ordering f(final BigDecimal a2) {
            final int x = a1.compareTo(a2);
            return x < 0 ? LT : x == 0 ? EQ : GT;
          }
        };
      }
    });

  /**
   * An order instance for the <code>long</code> type.
   */
  public static final Ord<Long> longOrd = new Ord<Long>(
    new F<Long, F<Long, Ordering>>() {
      public F<Long, Ordering> f(final Long a1) {
        return new F<Long, Ordering>() {
          public Ordering f(final Long a2) {
            final int x = a1.compareTo(a2);
            return x < 0 ? LT : x == 0 ? EQ : GT;
          }
        };
      }
    });

  /**
   * An order instance for the <code>short</code> type.
   */
  public static final Ord<Short> shortOrd = new Ord<Short>(
    new F<Short, F<Short, Ordering>>() {
      public F<Short, Ordering> f(final Short a1) {
        return new F<Short, Ordering>() {
          public Ordering f(final Short a2) {
            final int x = a1.compareTo(a2);
            return x < 0 ? LT : x == 0 ? EQ : GT;
          }
        };
      }
    });

  /**
   * An order instance for the {@link Ordering} type.
   */
  public static final Ord<Ordering> orderingOrd = new Ord<Ordering>(curry(new F2<Ordering, Ordering, Ordering>() {
    public Ordering f(final Ordering o1, final Ordering o2) {
      return o1 == o2 ?
        EQ :
        o1 == LT ?
          LT :
          o2 == LT ?
            GT :
            o1 == EQ ?
              LT :
              GT;
    }
  }));

  /**
   * An order instance for the {@link String} type.
   */
  public static final Ord<String> stringOrd = new Ord<String>(
    new F<String, F<String, Ordering>>() {
      public F<String, Ordering> f(final String a1) {
        return new F<String, Ordering>() {
          public Ordering f(final String a2) {
            final int x = a1.compareTo(a2);
            return x < 0 ? LT : x == 0 ? EQ : GT;
          }
        };
      }
    });

  /**
   * An order instance for the {@link StringBuffer} type.
   */
  public static final Ord<StringBuffer> stringBufferOrd = new Ord<StringBuffer>(new F<StringBuffer, F<StringBuffer, Ordering>>() {
    public F<StringBuffer, Ordering> f(final StringBuffer a1) {
      return new F<StringBuffer, Ordering>() {
        public Ordering f(final StringBuffer a2) {
          return stringOrd.compare(a1.toString(), a2.toString());
        }
      };
    }
  });

  /**
   * An order instance for the {@link StringBuffer} type.
   */
  public static final Ord<StringBuilder> stringBuilderOrd = new Ord<StringBuilder>(new F<StringBuilder, F<StringBuilder, Ordering>>() {
    public F<StringBuilder, Ordering> f(final StringBuilder a1) {
      return new F<StringBuilder, Ordering>() {
        public Ordering f(final StringBuilder a2) {
          return stringOrd.compare(a1.toString(), a2.toString());
        }
      };
    }
  });

  /**
   * An order instance for the {@link Option} type.
   *
   * @param oa Order across the element of the option.
   * @return An order instance for the {@link Option} type.
   */
  public static <A> Ord<Option<A>> optionOrd(final Ord<A> oa) {
    return new Ord<Option<A>>(new F<Option<A>, F<Option<A>, Ordering>>() {
      public F<Option<A>, Ordering> f(final Option<A> o1) {
        return new F<Option<A>, Ordering>() {
          public Ordering f(final Option<A> o2) {
            return o1.isNone() ?
              o2.isNone() ?
                EQ :
                LT :
              o2.isNone() ?
                GT :
                oa.f.f(o1.some()).f(o2.some());
          }
        };
      }
    });
  }

  /**
   * An order instance for the {@link Either} type.
   *
   * @param oa Order across the left side of {@link Either}.
   * @param ob Order across the right side of {@link Either}.
   * @return An order instance for the {@link Either} type.
   */
  public static <A, B> Ord<Either<A, B>> eitherOrd(final Ord<A> oa, final Ord<B> ob) {
    return new Ord<Either<A, B>>(new F<Either<A, B>, F<Either<A, B>, Ordering>>() {
      public F<Either<A, B>, Ordering> f(final Either<A, B> e1) {
        return new F<Either<A, B>, Ordering>() {
          public Ordering f(final Either<A, B> e2) {
            return e1.isLeft() ?
              e2.isLeft() ?
                oa.f.f(e1.left().value()).f(e2.left().value()) :
                LT :
              e2.isLeft() ?
                GT :
                ob.f.f(e1.right().value()).f(e2.right().value());
          }
        };
      }
    });
  }

  /**
   * An order instance for the {@link Validation} type.
   *
   * @param oa Order across the failing side of {@link Validation}.
   * @param ob Order across the succeeding side of {@link Validation}.
   * @return An order instance for the {@link Validation} type.
   */
  public static <A, B> Ord<Validation<A, B>> validationOrd(final Ord<A> oa, final Ord<B> ob) {
    return eitherOrd(oa, ob).comap(Validation.<A, B>either());
  }

  /**
   * An order instance for the {@link List} type.
   *
   * @param oa Order across the elements of the list.
   * @return An order instance for the {@link List} type.
   */
  public static <A> Ord<List<A>> listOrd(final Ord<A> oa) {
    return new Ord<List<A>>(new F<List<A>, F<List<A>, Ordering>>() {
      public F<List<A>, Ordering> f(final List<A> l1) {
        return new F<List<A>, Ordering>() {
          public Ordering f(final List<A> l2) {
            if (l1.isEmpty())
              return l2.isEmpty() ? EQ : LT;
            else if (l2.isEmpty())
              return l1.isEmpty() ? EQ : GT;
            else {
              final Ordering c = oa.compare(l1.head(), l2.head());
              return c == EQ ? listOrd(oa).f.f(l1.tail()).f(l2.tail()) : c;
            }
          }
        };
      }
    });
  }

  /**
   * An order instance for the {@link NonEmptyList} type.
   *
   * @param oa Order across the elements of the non-empty list.
   * @return An order instance for the {@link NonEmptyList} type.
   */
  public static <A> Ord<NonEmptyList<A>> nonEmptyListOrd(final Ord<A> oa) {
    return listOrd(oa).comap(NonEmptyList.<A>toList_());
  }

  /**
   * An order instance for the {@link Stream} type.
   *
   * @param oa Order across the elements of the stream.
   * @return An order instance for the {@link Stream} type.
   */
  public static <A> Ord<Stream<A>> streamOrd(final Ord<A> oa) {
    return new Ord<Stream<A>>(new F<Stream<A>, F<Stream<A>, Ordering>>() {
      public F<Stream<A>, Ordering> f(final Stream<A> s1) {
        return new F<Stream<A>, Ordering>() {
          public Ordering f(final Stream<A> s2) {
            if (s1.isEmpty())
              return s2.isEmpty() ? EQ : LT;
            else if (s2.isEmpty())
              return s1.isEmpty() ? EQ : GT;
            else {
              final Ordering c = oa.compare(s1.head(), s2.head());
              return c == EQ ? streamOrd(oa).f.f(s1.tail()._1()).f(s2.tail()._1()) : c;
            }
          }
        };
      }
    });
  }

  /**
   * An order instance for the {@link Array} type.
   *
   * @param oa Order across the elements of the array.
   * @return An order instance for the {@link Array} type.
   */
  public static <A> Ord<Array<A>> arrayOrd(final Ord<A> oa) {
    return new Ord<Array<A>>(new F<Array<A>, F<Array<A>, Ordering>>() {
      public F<Array<A>, Ordering> f(final Array<A> a1) {
        return new F<Array<A>, Ordering>() {
          public Ordering f(final Array<A> a2) {
            int i = 0;
            //noinspection ForLoopWithMissingComponent
            for (; i < a1.length() && i < a2.length(); i++) {
              final Ordering c = oa.compare(a1.get(i), a2.get(i));
              if (c == GT || c == LT)
                return c;
            }
            return i == a1.length() ?
              i == a2.length() ?
                EQ :
                LT :
              i == a1.length() ?
                EQ :
                GT;
          }
        };
      }
    });
  }

  /**
   * An order instance for the {@link Set} type.
   *
   * @param oa Order across the elements of the set.
   * @return An order instance for the {@link Set} type.
   */
  public static <A> Ord<Set<A>> setOrd(final Ord<A> oa) {
    return streamOrd(oa).comap(new F<Set<A>, Stream<A>>() {
      public Stream<A> f(final Set<A> as) {
        return as.toStream();
      }
    });
  }
}
