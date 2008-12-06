package fj.pre;

import fj.F;
import fj.F2;
import static fj.Function.curry;
import fj.P1;
import fj.P2;
import fj.Unit;
import fj.data.Array;
import fj.data.List;
import fj.data.NonEmptyList;
import fj.data.Option;
import fj.data.Set;
import fj.data.Stream;

import java.math.BigInteger;
import java.math.BigDecimal;

/**
 * Implementations must satisfy the law of associativity:
 * <ul>
 * <li><em>Associativity</em>; forall x. forall y. forall z. sum(sum(x, y), z) == sum(x, sum(y, z))</li>
 * </ul>
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class Semigroup<A> {
  private final F<A, F<A, A>> sum;

  private Semigroup(final F<A, F<A, A>> sum) {
    this.sum = sum;
  }

  /**
   * Sums the two given arguments.
   *
   * @param a1 A value to sum with another.
   * @param a2 A value to sum with another.
   * @return The of the two given arguments.
   */
  public A sum(final A a1, final A a2) {
    return sum.f(a1).f(a2);
  }

  /**
   * Returns a function that sums the given value according to this semigroup.
   *
   * @param a1 The value to sum.
   * @return A function that sums the given value according to this semigroup.
   */
  public F<A, A> sum(final A a1) {
    return sum.f(a1);
  }

  /**
   * Returns a function that sums according to this semigroup.
   *
   * @return A function that sums according to this semigroup.
   */
  public F<A, F<A, A>> sum() {
    return sum;
  }

  /**
   * Constructs a semigroup from the given function.
   *
   * @param sum The function to construct this semigroup with.
   * @return A semigroup from the given function.
   */
  public static <A> Semigroup<A> semigroup(final F<A, F<A, A>> sum) {
    return new Semigroup<A>(sum);
  }

  /**
   * Constructs a semigroup from the given function.
   *
   * @param sum The function to construct this semigroup with.
   * @return A semigroup from the given function.
   */
  public static <A> Semigroup<A> semigroup(final F2<A, A, A> sum) {
    return new Semigroup<A>(curry(sum));
  }

  /**
   * A semigroup that adds integers.
   */
  public static final Semigroup<Integer> intAdditionSemigroup = semigroup(new F2<Integer, Integer, Integer>() {
    public Integer f(final Integer i1, final Integer i2) {
      return i1 + i2;
    }
  });

  /**
   * A semigroup that multiplies integers.
   */
  public static final Semigroup<Integer> intMultiplicationSemigroup = semigroup(new F2<Integer, Integer, Integer>() {
    public Integer f(final Integer i1, final Integer i2) {
      return i1 * i2;
    }
  });

  /**
   * A semigroup that adds big integers.
   */
  public static final Semigroup<BigInteger> bigintAdditionSemigroup = semigroup(new F2<BigInteger, BigInteger, BigInteger>() {
    public BigInteger f(final BigInteger i1, final BigInteger i2) {
      return i1.add(i2);
    }
  });

  /**
   * A semigroup that multiplies big integers.
   */
  public static final Semigroup<BigInteger> bigintMultiplicationSemigroup = semigroup(new F2<BigInteger, BigInteger, BigInteger>() {
    public BigInteger f(final BigInteger i1, final BigInteger i2) {
      return i1.multiply(i2);
    }
  });

  /**
   * A semigroup that adds big decimals.
   */
  public static final Semigroup<BigDecimal> bigdecimalAdditionSemigroup = semigroup(new F2<BigDecimal, BigDecimal, BigDecimal>() {
    public BigDecimal f(final BigDecimal i1, final BigDecimal i2) {
      return i1.add(i2);
    }
  });

  /**
   * A semigroup that multiplies big decimals.
   */
  public static final Semigroup<BigDecimal> bigdecimalMultiplicationSemigroup = semigroup(new F2<BigDecimal, BigDecimal, BigDecimal>() {
    public BigDecimal f(final BigDecimal i1, final BigDecimal i2) {
      return i1.multiply(i2);
    }
  });

  /**
   * A semigroup that adds longs.
   */
  public static final Semigroup<Long> longAdditionSemigroup = semigroup(new F2<Long, Long, Long>() {
    public Long f(final Long x, final Long y) {
      return x + y;
    }
  });

  /**
   * A semigroup that multiplies longs.
   */
  public static final Semigroup<Long> longMultiplicationSemigroup = semigroup(new F2<Long, Long, Long>() {
    public Long f(final Long x, final Long y) {
      return x * y;
    }
  });

  /**
   * A semigroup that ORs booleans.
   */
  public static final Semigroup<Boolean> disjunctionSemigroup = semigroup(new F2<Boolean, Boolean, Boolean>() {
    public Boolean f(final Boolean b1, final Boolean b2) {
      return b1 || b2;
    }
  });

  /**
   * A semigroup that XORs booleans.
   */
  public static final Semigroup<Boolean> exclusiveDisjunctionSemiGroup = semigroup(new F2<Boolean, Boolean, Boolean>() {
    public Boolean f(final Boolean p, final Boolean q) {
      return p && !q || !p && q;
    }
  });

  /**
   * A semigroup that ANDs booleans.
   */
  public static final Semigroup<Boolean> conjunctionSemigroup = semigroup(new F2<Boolean, Boolean, Boolean>() {
    public Boolean f(final Boolean b1, final Boolean b2) {
      return b1 && b2;
    }
  });

  /**
   * A semigroup that appends strings.
   */
  public static final Semigroup<String> stringSemigroup = semigroup(new F2<String, String, String>() {
    public String f(final String s1, final String s2) {
      return s1 + s2;
    }
  });

  /**
   * A semigroup that appends string buffers.
   */
  public static final Semigroup<StringBuffer> stringBufferSemigroup = semigroup(new F2<StringBuffer, StringBuffer, StringBuffer>() {
    public StringBuffer f(final StringBuffer s1, final StringBuffer s2) {
      return new StringBuffer(s1).append(s2);
    }
  });

  /**
   * A semigroup that appends string builders.
   */
  public static final Semigroup<StringBuilder> stringBuilderSemigroup = semigroup(new F2<StringBuilder, StringBuilder, StringBuilder>() {
    public StringBuilder f(final StringBuilder s1, final StringBuilder s2) {
      return new StringBuilder(s1).append(s2);
    }
  });

  /**
   * A semigroup for functions.
   *
   * @param sb The smeigroup for the codomain.
   * @return A semigroup for functions.
   */
  public static <A, B> Semigroup<F<A, B>> functionSemigroup(final Semigroup<B> sb) {
    return semigroup(new F2<F<A, B>, F<A, B>, F<A, B>>() {
      public F<A, B> f(final F<A, B> a1, final F<A, B> a2) {
        return new F<A, B>() {
          public B f(final A a) {
            return sb.sum(a1.f(a), a2.f(a));
          }
        };
      }
    });
  }

  /**
   * A semigroup for lists.
   *
   * @return A semigroup for lists.
   */
  public static <A> Semigroup<List<A>> listSemigroup() {
    return semigroup(new F2<List<A>, List<A>, List<A>>() {
      public List<A> f(final List<A> a1, final List<A> a2) {
        return a1.append(a2);
      }
    });
  }

  /**
   * A semigroup for non-empty lists.
   *
   * @return A semigroup for non-empty lists.
   */
  public static <A> Semigroup<NonEmptyList<A>> nonEmptyListSemigroup() {
    return semigroup(new F2<NonEmptyList<A>, NonEmptyList<A>, NonEmptyList<A>>() {
      public NonEmptyList<A> f(final NonEmptyList<A> a1, final NonEmptyList<A> a2) {
        return a1.append(a2);
      }
    });
  }

  /**
   * A semigroup for optional values.
   *
   * @return A semigroup for optional values.
   */
  public static <A> Semigroup<Option<A>> optionSemigroup() {
    return semigroup(new F2<Option<A>, Option<A>, Option<A>>() {
      public Option<A> f(final Option<A> a1, final Option<A> a2) {
        return a1.isSome() ? a1 : a2;
      }
    });
  }

  /**
   * A semigroup for streams.
   *
   * @return A semigroup for streams.
   */
  public static <A> Semigroup<Stream<A>> streamSemigroup() {
    return semigroup(new F2<Stream<A>, Stream<A>, Stream<A>>() {
      public Stream<A> f(final Stream<A> a1, final Stream<A> a2) {
        return a1.append(a2);
      }
    });
  }

  /**
   * A semigroup for arrays.
   *
   * @return A semigroup for arrays.
   */
  public static <A> Semigroup<Array<A>> arraySemigroup() {
    return semigroup(new F2<Array<A>, Array<A>, Array<A>>() {
      public Array<A> f(final Array<A> a1, final Array<A> a2) {
        return a1.append(a2);
      }
    });
  }

  /**
   * A semigroup for unary products.
   *
   * @param sa A semigroup for the product's type.
   * @return A semigroup for unary products.
   */
  public static <A> Semigroup<P1<A>> p1Semigroup(final Semigroup<A> sa) {
    return semigroup(new F2<P1<A>, P1<A>, P1<A>>() {
      public P1<A> f(final P1<A> a1, final P1<A> a2) {
        return new P1<A>() {
          public A _1() {
            return sa.sum(a1._1(), a2._1());
          }
        };
      }
    });
  }

  /**
   * A semigroup for binary products.
   *
   * @param sa A semigroup for the product's first type.
   * @param sb A semigroup for the product's second type.
   * @return A semigroup for binary products.
   */
  public static <A, B> Semigroup<P2<A, B>> p2Semigroup(final Semigroup<A> sa, final Semigroup<B> sb) {
    return semigroup(new F2<P2<A, B>, P2<A, B>, P2<A, B>>() {
      public P2<A, B> f(final P2<A, B> a1, final P2<A, B> a2) {
        return new P2<A, B>() {
          public A _1() {
            return sa.sum(a1._1(), a2._1());
          }

          public B _2() {
            return sb.sum(a1._2(), a2._2());
          }
        };
      }
    });
  }

  /**
   * A semigroup for the Unit value.
   */
  public static final Semigroup<Unit> unitSemigroup = semigroup(new F2<Unit, Unit, Unit>() {
    public Unit f(final Unit u1, final Unit u2) {
      return Unit.unit();
    }
  });

  /**
   * A semigroup for sets.
   *
   * @return a semigroup for sets.
   */
  public static <A> Semigroup<Set<A>> setSemigroup() {
    return semigroup(new F2<Set<A>, Set<A>, Set<A>>() {
      public Set<A> f(final Set<A> a, final Set<A> b) {
        return a.union(b);
      }
    });
  }

}
