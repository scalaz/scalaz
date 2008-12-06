package fj.test;

import static fj.Bottom.error;
import fj.Effect;
import fj.F;
import fj.Function;
import static fj.Function.flip;
import static fj.Function.curry;
import fj.P2;
import static fj.P2.__1;
import fj.Unit;
import fj.F2;
import static fj.data.Array.array;
import fj.data.List;
import static fj.data.List.nil;
import static fj.data.List.replicate;
import fj.data.Option;
import static fj.pre.Monoid.intAdditionMonoid;
import static fj.pre.Ord.intOrd;

import static java.lang.Math.max;
import static java.lang.Math.min;

/**
 * <p> A generator for values of the type of the given type parameter (<code>A</code>). Generation
 * of a value accepts a general 'size' argument (integer), a {@link Rand random generator} and
 * returns an {@link Option optional value} of the type parameter. Several generators are provided,
 * allowing various forms of composition of generators. </p> <p> A user typically creates an {@link
 * Arbitrary arbitrary} to return a generator using the 'combinator methods' below. For example,
 * suppose a <code>class Person</code>:
<pre>
class Person {
  final int age;
  final String name;
  final boolean male;

  Person(final int age, final String name, final boolean male) {
    this.age = age;
    this.name = name;
    this.male = male;
  }
}
</pre>
 * </p> <p> In a case like this one, a user may create a generator over <code>Person</code> by
 * invoking the {@link #bind(F)}  methods &mdash; in this case, {@link #bind(Gen , Gen , F)} the one
 * that takes two generator arguments}, since the class has one more than two fields (the bind
 * method is invoked on a generator adding the extra one to the count as they are composed). The
 * class fields are of types for which there exist generators (on {@link Arbitrary} so those can be
 * used to compose a generator for <code>Person</code>: </p>
<pre>
static Arbitrary&lt;Person&gt; personArbitrary() {
  final Gen&lt;Person&gt; personGenerator = arbInteger().gen().bind(arbString().gen(), arbBoolean().gen(),
      // compose the generators
      {int age =&gt; {String name =&gt; {boolean male =&gt; new Person(age, name, male)}}};
  return arbitrary(personGenerator);
}
</pre>
 * <p/>
 * The example above uses Java 7 closure syntax. Here is the same example using objects instead:
<pre>
static Arbitrary&lt;Person&gt; personArbitrary() {
  final Gen&lt;Person&gt; personGenerator = arbInteger().gen().bind(arbString().gen(), arbBoolean().gen(),
      // compose the generators
      new F&lt;Integer, F&lt;String, F&lt;Boolean, Person&gt;&gt;&gt;() {
        public F&lt;String, F&lt;Boolean, Person&gt;&gt; f(final Integer age) {
          return new F&lt;String, F&lt;Boolean, Person&gt;&gt;() {
            public F&lt;Boolean, Person&gt; f(final String name) {
              return new F&lt;Boolean, Person&gt;() {
                public Person f(final Boolean male) {
                  return new Person(age, name, male);
                }
              };
            }
          };
        }
      });
  return arbitrary(personGenerator);
}
</pre>
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          <li>$LastChangedBy$</li>
 *          </ul>
 */
public final class Gen<A> {
  private final F<Integer, F<Rand, A>> f;

  private Gen(final F<Integer, F<Rand, A>> f) {
    this.f = f;
  }

  /**
   * Applies the given size and random generator to produce a value.
   *
   * @param i The size to use to produce the value.
   * @param r The random generator to use to produce the value..
   * @return A generated value.
   */
  public A gen(final int i, final Rand r) {
    return f.f(i).f(r);
  }

  /**
   * Maps the given function across this generator.
   *
   * @param f The function to map across this generator.
   * @return A new generator after applying the mapping function.
   */
  public <B> Gen<B> map(final F<A, B> f) {
    return new Gen<B>(new F<Integer, F<Rand, B>>() {
      public F<Rand, B> f(final Integer i) {
        return new F<Rand, B>() {
          public B f(final Rand r) {
            return f.f(gen(i, r));
          }
        };
      }
    });
  }

  /**
   * Returns a generator that produces values that meet the given predicate.
   *
   * @param f The predicate to meet for the values produced by the generator.
   * @return A generator that produces values that meet the given predicate.
   */
  public Gen<A> filter(final F<A, Boolean> f) {
    return gen(curry(new F2<Integer, Rand, A>() {
      public A f(final Integer i, final Rand r) {
        A a;

        do {
          a = gen(i, r);
        } while(!f.f(a));

        return a;
      }
    }));
  }

  /**
   * Executes a side-effect for each generated result using the given arguments.
   *
   * @param i The size to generate the result to apply the side-effect to.
   * @param r The random generator to generate the result to apply the side-effect to.
   * @param f The side-effect to execute on the generated value.
   * @return The unit value.
   */
  public Unit foreach(final Integer i, final Rand r, final F<A, Unit> f) {
    return f.f(this.f.f(i).f(r));
  }

  /**
   * Executes a side-effect for each generated result using the given arguments.
   *
   * @param i The size to generate the result to apply the side-effect to.
   * @param r The random generator to generate the result to apply the side-effect to.
   * @param f The side-effect to execute on the generated value.
   */
  public void foreach(final Integer i, final Rand r, final Effect<A> f) {
    f.e(this.f.f(i).f(r));
  }

  /**
   * Binds the given function across this generator to produce a new generator.
   *
   * @param f The function to bind across this generator.
   * @return A new generator after binding the given function.
   */
  public <B> Gen<B> bind(final F<A, Gen<B>> f) {
    return new Gen<B>(new F<Integer, F<Rand, B>>() {
      public F<Rand, B> f(final Integer i) {
        return new F<Rand, B>() {
          public B f(final Rand r) {            
            return f.f(gen(i, r)).f.f(i).f(r);
          }
        };
      }
    });
  }

  /**
   * Binds the given function across this generator and the given generator to produce a new
   * generator.
   *
   * @param gb The second generator to bind the given function across.
   * @param f  The function to bind across this generator and the given generator.
   * @return A new generator after binding the given function.
   */
  public <B, C> Gen<C> bind(final Gen<B> gb, final F<A, F<B, C>> f) {
    return gb.apply(map(f));
  }

  /**
   * Binds the given function across this generator and the given generators to produce a new
   * generator.
   *
   * @param gb The second generator to bind the given function across.
   * @param gc The third generator to bind the given function across.
   * @param f  The function to bind across this generator and the given generators.
   * @return A new generator after binding the given function.
   */
  public <B, C, D> Gen<D> bind(final Gen<B> gb, final Gen<C> gc, final F<A, F<B, F<C, D>>> f) {
    return gc.apply(bind(gb, f));
  }

  /**
   * Binds the given function across this generator and the given generators to produce a new
   * generator.
   *
   * @param gb The second generator to bind the given function across.
   * @param gc The third generator to bind the given function across.
   * @param gd The fourth generator to bind the given function across.
   * @param f  The function to bind across this generator and the given generators.
   * @return A new generator after binding the given function.
   */
  public <B, C, D, E> Gen<E> bind(final Gen<B> gb, final Gen<C> gc, final Gen<D> gd, final F<A, F<B, F<C, F<D, E>>>> f) {
    return gd.apply(bind(gb, gc, f));
  }

  /**
   * Binds the given function across this generator and the given generators to produce a new
   * generator.
   *
   * @param gb The second generator to bind the given function across.
   * @param gc The third generator to bind the given function across.
   * @param gd The fourth generator to bind the given function across.
   * @param ge The fifth generator to bind the given function across.
   * @param f  The function to bind across this generator and the given generators.
   * @return A new generator after binding the given function.
   */
  public <B, C, D, E, F$> Gen<F$> bind(final Gen<B> gb, final Gen<C> gc, final Gen<D> gd, final Gen<E> ge, final F<A, F<B, F<C, F<D, F<E, F$>>>>> f) {
    return ge.apply(bind(gb, gc, gd, f));
  }

  /**
   * Binds the given function across this generator and the given generators to produce a new
   * generator.
   *
   * @param gb The second generator to bind the given function across.
   * @param gc The third generator to bind the given function across.
   * @param gd The fourth generator to bind the given function across.
   * @param ge The fifth generator to bind the given function across.
   * @param gf The sixth generator to bind the given function across.
   * @param f  The function to bind across this generator and the given generators.
   * @return A new generator after binding the given function.
   */
  public <B, C, D, E, F$, G> Gen<G> bind(final Gen<B> gb, final Gen<C> gc, final Gen<D> gd, final Gen<E> ge, final Gen<F$> gf, final F<A, F<B, F<C, F<D, F<E, F<F$, G>>>>>> f) {
    return gf.apply(bind(gb, gc, gd, ge, f));
  }

  /**
   * Binds the given function across this generator and the given generators to produce a new
   * generator.
   *
   * @param gb The second generator to bind the given function across.
   * @param gc The third generator to bind the given function across.
   * @param gd The fourth generator to bind the given function across.
   * @param ge The fifth generator to bind the given function across.
   * @param gf The sixth generator to bind the given function across.
   * @param gg The seventh generator to bind the given function across.
   * @param f  The function to bind across this generator and the given generators.
   * @return A new generator after binding the given function.
   */
  public <B, C, D, E, F$, G, H> Gen<H> bind(final Gen<B> gb, final Gen<C> gc, final Gen<D> gd, final Gen<E> ge, final Gen<F$> gf, final Gen<G> gg, final F<A, F<B, F<C, F<D, F<E, F<F$, F<G, H>>>>>>> f) {
    return gg.apply(bind(gb, gc, gd, ge, gf, f));
  }

  /**
   * Binds the given function across this generator and the given generators to produce a new
   * generator.
   *
   * @param gb The second generator to bind the given function across.
   * @param gc The third generator to bind the given function across.
   * @param gd The fourth generator to bind the given function across.
   * @param ge The fifth generator to bind the given function across.
   * @param gf The sixth generator to bind the given function across.
   * @param gg The seventh generator to bind the given function across.
   * @param gh The eighth generator to bind the given function across.
   * @param f  The function to bind across this generator and the given generators.
   * @return A new generator after binding the given function.
   */
  public <B, C, D, E, F$, G, H, I> Gen<I> bind(final Gen<B> gb, final Gen<C> gc, final Gen<D> gd, final Gen<E> ge, final Gen<F$> gf, final Gen<G> gg, final Gen<H> gh, final F<A, F<B, F<C, F<D, F<E, F<F$, F<G, F<H, I>>>>>>>> f) {
    return gh.apply(bind(gb, gc, gd, ge, gf, gg, f));
  }

  /**
   * Function application within this generator to produce a new generator.
   *
   * @param gf The generator over the function to apply to this generator.
   * @return A new generator after function application.
   */
  public <B> Gen<B> apply(final Gen<F<A, B>> gf) {
    return gf.bind(new F<F<A, B>, Gen<B>>() {
      public Gen<B> f(final F<A, B> f) {
        return map(new F<A, B>() {
          public B f(final A a) {
            return f.f(a);
          }
        });
      }
    });
  }

  /**
   * Resizes this generator with the given size.
   *
   * @param s The new size of the generator.
   * @return A new generator that uses the given size.
   */
  public Gen<A> resize(final int s) {
    return new Gen<A>(new F<Integer, F<Rand, A>>() {
      public F<Rand, A> f(final Integer i) {
        return new F<Rand, A>() {
          public A f(final Rand r) {
            return f.f(s).f(r);
          }
        };
      }
    });
  }

  /**
   * Returns a generator that uses the given function.
   *
   * @param f The function to use for this generator.
   * @return A new generator that uses the given function.
   */
  public static <A> Gen<A> gen(final F<Integer, F<Rand, A>> f) {
    return new Gen<A>(f);
  }

  /**
   * Sequence the given generators through a {@link #bind(F)} operation.
   *
   * @param gs The generators to sequence.
   * @return A generator of lists after sequencing the given generators.
   */
  public static <A> Gen<List<A>> sequence(final List<Gen<A>> gs) {
    return gs.foldRight(new F<Gen<A>, F<Gen<List<A>>, Gen<List<A>>>>() {
      public F<Gen<List<A>>, Gen<List<A>>> f(final Gen<A> ga) {
        return new F<Gen<List<A>>, Gen<List<A>>>() {
          public Gen<List<A>> f(final Gen<List<A>> gas) {
            return ga.bind(gas, List.<A>cons());
          }
        };
      }
    }, value(List.<A>nil()));
  }

  /**
   * Sequences the given generator the given number of times through a {@link #bind(F)} operation.
   *
   * @param n The number of times to sequence the given generator.
   * @param g The generator sequence.
   * @return A generator of lists after sequencing the given generator.
   */
  public static <A> Gen<List<A>> sequenceN(final int n, final Gen<A> g) {
    return sequence(replicate(n, g));
  }

  /**
   * Constructs a generator that can access its construction arguments &mdash; size and random
   * generator.
   *
   * @param f The function that constructs the generator with its arguments.
   * @return A new generator.
   */
  public static <A> Gen<A> parameterised(final F<Integer, F<Rand, Gen<A>>> f) {
    return new Gen<A>(curry(new F2<Integer, Rand, A>() {
      public A f(final Integer i, Rand r) {
        return f.f(i).f(r).gen(i, r);
      }
    }));
  }

  /**
   * Constructs a generator that can access its size construction arguments.
   *
   * @param f The function that constructs the generator with its size argument.
   * @return A new generator.
   */
  public static <A> Gen<A> sized(final F<Integer, Gen<A>> f) {
    return parameterised(flip(Function.<Rand, F<Integer, Gen<A>>>constant(f)));
  }

  /**
   * Returns a generator that always produces the given value.
   *
   * @param a The value to always produce.
   * @return A generator that always produces the given value.
   */
  public static <A> Gen<A> value(final A a) {
    return new Gen<A>(new F<Integer, F<Rand, A>>() {
      public F<Rand, A> f(final Integer i) {
        return new F<Rand, A>() {
          public A f(final Rand r) {
            return a;
          }
        };
      }
    });
  }

  /**
   * Returns a generator that produces values between the given range (inclusive).
   *
   * @param from The value for the generator to produce values from.
   * @param to   The value for the generator to produce values from.
   * @return A generator that produces values between the given range (inclusive).
   */
  public static Gen<Integer> choose(final int from, final int to) {
    final int f = min(from, to);
    final int t = max(from, to);
    return parameterised(curry(new F2<Integer, Rand, Gen<Integer>>() {
      public Gen<Integer> f(final Integer i, final Rand r) {
        return value(r.choose(f, t));
      }
    }));
  }

  /**
   * Returns a generator that produces values between the given range (inclusive).
   *
   * @param from The value for the generator to produce values from.
   * @param to   The value for the generator to produce values from.
   * @return A generator that produces v
   */
  public static Gen<Double> choose(final double from, final double to) {
    final double f = min(from, to);
    final double t = max(from, to);
    return parameterised(new F<Integer, F<Rand, Gen<Double>>>() {
      public F<Rand, Gen<Double>> f(final Integer i) {
        return new F<Rand, Gen<Double>>() {
          public Gen<Double> f(final Rand r) {
            return value(r.choose(f, t));
          }
        };
      }
    });
  }

  /**
   * Returns a generator that never returns a value.
   *
   * @return A generator that never returns a value.
   */
  public static <A> Gen<A> fail() {
    return new Gen<A>(new F<Integer, F<Rand, A>>() {
      public F<Rand, A> f(final Integer i) {
        return new F<Rand, A>() {
          public A f(final Rand r) {
            throw error("Failing generator");
          }
        };
      }
    });
  }

  /**
   * Joins the generator of generators through a {@link #bind(F)} operation.
   *
   * @param g The generator of generators to join.
   * @return A new generator after joining the given generator.
   */
  public static <A> Gen<A> join(final Gen<Gen<A>> g) {
    return g.bind(Function.<Gen<A>>identity());
  }

  /**
   * Returns a generator that uses values from the given frequency and generator pairs. The returned
   * generator will produce values from the generator in a pair with a higher frequency than a lower
   * frequency generator.
   *
   * @param gs The pairs of frequency and generator from which to return values in the returned
   *           generator.
   * @return A new generator that uses the given pairs of frequency and generator.
   */
  public static <A> Gen<A> frequency(final List<P2<Integer, Gen<A>>> gs) {
    final class Pick {
      <A> Gen<A> pick(final int n, final List<P2<Integer, Gen<A>>> gs) {
        if(gs.isEmpty())
          return fail();
        else {
          final int k = gs.head()._1();
          return n <= k ? gs.head()._2() : pick(n - k, gs.tail());
        }
      }
    }

    final F<P2<Integer, Gen<A>>, Integer> f = __1();

    return choose(1, intAdditionMonoid.sumLeft(gs.map(f))).bind(new F<Integer, Gen<A>>() {
      public Gen<A> f(final Integer i) {
        return new Pick().pick(i, gs);
      }
    });
  }

  /**
   * Returns a generator that produces values from the given frequency and value pairs. The returned
   * generator will produce the value with a higher frequency than a lower one.
   *
   * @param as The pairs of frequency and value from which to produce values.
   * @return A new generator that uses the given pairs of frequency and value.
   */
  public static <A> Gen<A> elemFrequency(final List<P2<Integer, A>> as) {
    return frequency(as.map(new F<P2<Integer, A>, P2<Integer, Gen<A>>>() {
      public P2<Integer, Gen<A>> f(final P2<Integer, A> p) {
        return p.map2(new F<A, Gen<A>>() {
          public Gen<A> f(final A a) {
            return value(a);
          }
        });
      }
    }));
  }

  /**
   * Returns a generator that produces values from the given arguments.
   *
   * @param as The values that the returned generator may produce.
   * @return A generator that produces values from the given arguments.
   */
  public static <A> Gen<A> elements(final A... as) {
    return array(as).isEmpty() ? Gen.<A>fail() : choose(0, as.length - 1).map(new F<Integer, A>() {
      public A f(final Integer i) {
        return as[i];
      }
    });
  }

  /**
   * Returns a generator that produces values from one of the given generators on subsequent
   * requests.
   *
   * @param gs The list of generators to produce a value from.
   * @return A generator that produces values from one of the given generators on subsequent
   *         requests.
   */
  public static <A> Gen<A> oneOf(final List<Gen<A>> gs) {
    return gs.isEmpty() ? Gen.<A>fail() : choose(0, gs.length() - 1).bind(new F<Integer, Gen<A>>() {
      public Gen<A> f(final Integer i) {
        return gs.index(i);
      }
    });
  }

  /**
   * Returns a generator of lists whose values come from the given generator.
   *
   * @param g The generator to produce values from for the returned generator.
   * @param x An adjuster of size to apply to the given generator when producing values.
   * @return A generator of lists whose values come from the given generator.
   */
  public static <A> Gen<List<A>> listOf(final Gen<A> g, final int x) {
    return sized(new F<Integer, Gen<List<A>>>() {
      public Gen<List<A>> f(final Integer size) {
        return choose(x, size).bind(new F<Integer, Gen<List<A>>>() {
          public Gen<List<A>> f(final Integer n) {
            return sequenceN(n, g);
          }
        });
      }
    });
  }

  /**
   * Returns a generator of lists whose values come from the given generator.
   *
   * @param g The generator to produce values from for the returned generator.
   * @return A generator of lists whose values come from the given generator.
   */
  public static <A> Gen<List<A>> listOf(final Gen<A> g) {
    return listOf(g, 0);
  }

  /**
   * Returns a generator of lists whose values come from the given generator.
   *
   * @param g The generator to produce values from for the returned generator.
   * @return A generator of lists whose values come from the given generator.
   */
  public static <A> Gen<List<A>> listOf1(final Gen<A> g) {
    return listOf(g, 1);
  }

  /**
   * Returns a generator of lists that picks the given number of elements from the given list. If
   * the given number is less than zero or greater than the length of the given list, then the
   * returned generator will never produce a value.
   *
   * @param n  The number of elements to pick from the given list.
   * @param as The list from which to pick elements.
   * @return A generator of lists that picks the given number of elements from the given list.
   */
  public static <A> Gen<List<A>> pick(final int n, final List<A> as) {
    return n < 0 || n > as.length() ? Gen.<List<A>>fail() : sequenceN(n, choose(0, as.length() - 1)).map(new F<List<Integer>, List<A>>() {
      public List<A> f(final List<Integer> is) {
        List<A> r = nil();

        List<Integer> iis = is.sort(intOrd);
        List<P2<A, Integer>> aas = as.zipIndex();

        //noinspection ForLoopWithMissingComponent
        for(; iis.isNotEmpty() && aas.isNotEmpty(); aas = aas.tail())
          if(iis.head().equals(aas.head()._2()))
            iis = iis.tail();
          else
            r = r.snoc(aas.head()._1());

        return r;
      }
    });
  }

  /**
   * Returns a generator of lists that produces some of the values of the given list.
   *
   * @param as The list from which to pick values.
   * @return A generator of lists that produces some of the values of the given list.
   */
  public static <A> Gen<List<A>> someOf(final List<A> as) {
    return choose(0, as.length()).bind(new F<Integer, Gen<List<A>>>() {
      public Gen<List<A>> f(final Integer i) {
        return pick(i, as);
      }
    });
  }

  /**
   * Promotes the given function to a generator for functions.
   *
   * @param f The function to promote to a generator of functions.
   * @return A generator for functions.
   */
  public static <A, B> Gen<F<A, B>> promote(final F<A, Gen<B>> f) {
    return new Gen<F<A, B>>(new F<Integer, F<Rand, F<A, B>>>() {
      public F<Rand, F<A, B>> f(final Integer i) {
        return new F<Rand, F<A, B>>() {
          public F<A, B> f(final Rand r) {
            return new F<A, B>() {
              public B f(final A a) {
                return f.f(a).f.f(i).f(r);
              }
            };
          }
        };
      }
    });
  }
}
