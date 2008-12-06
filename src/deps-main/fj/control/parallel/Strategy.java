package fj.control.parallel;

import fj.Effect;
import fj.F;
import fj.F2;
import fj.Function;
import fj.P;
import fj.P1;
import static fj.Function.compose;
import static fj.Function.curry;
import static fj.P1.fmap;
import static fj.P1.sequence;
import fj.data.Java;
import fj.data.List;
import fj.data.Array;

import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;

/**
 * Functional-style parallel evaluation strategies.
 * A Strategy is a method of evaluating a product-1, yielding another product-1 from which the result of its evaluation
 * can be retrieved at a later time.
 * <p/>
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          <li>Author: runar</li>
 *          </ul>
 */
public final class Strategy<A> {

  private final F<P1<A>, P1<A>> f;

  private Strategy(final F<P1<A>, P1<A>> f) {
    this.f = f;
  }

  /**
   * Returns the functional representation of this Strategy, a function that evaluates a product-1.
   *
   * @return The function representing this strategy, which evaluates a product-1.
   */
  public F<P1<A>, P1<A>> f() {
    return f;
  }

  /**
   * Constructs a strategy from the given evaluation function.
   *
   * @param f The execution function for the strategy
   * @return A strategy that uses the given function to evaluate product-1s.
   */
  public static <A> Strategy<A> strategy(final F<P1<A>, P1<A>> f) {
    return new Strategy<A>(f);
  }

  /**
   * Apply the strategy to the given product-1.
   *
   * @param a A P1 to evaluate according to this strategy.
   * @return A P1 that yields the value from calling the given product-1.
   */
  public P1<A> par(final P1<A> a) {
    return f().f(a);
  }

  /**
   * Promotes a function to a concurrent function.
   *
   * @param f A function to promote to a concurrent function.
   * @return A function that executes concurrently when called, yielding a Future value.
   */
  public <B> F<B, P1<A>> concurry(final F<B, A> f) {
    return compose(f(), P1.<B, A>curry(f));
  }

  /**
   * Promotes a function of arity-2 to a concurrent function.
   *
   * @param f The function to promote to a concurrent function.
   * @return A function that executes concurrently when called, yielding a product-1 that returns the value.
   */
  public <B, C> F<B, F<C, P1<A>>> concurry(final F2<B, C, A> f) {
    return new F<B, F<C, P1<A>>>() {
      public F<C, P1<A>> f(final B b) {
        return concurry(curry(f).f(b));
      }
    };
  }

  /**
   * Waits for every Future in a list to obtain a value, and collects those values in a list.
   *
   * @param xs The list of Futures from which to get values.
   * @return A list of values extracted from the Futures in the argument list.
   */
  public static <A> List<P1<A>> mergeAll(final List<Future<A>> xs) {
    return xs.map(Strategy.<A>obtain());
  }

  /**
   * Evaluates a list of product-1s in parallel.
   *
   * @param ps A list to evaluate in parallel.
   * @return A list of the values of the product-1s in the argument.
   */
  public P1<List<A>> parList(final List<P1<A>> ps) {
    return sequence(ps.map(f()));
  }

  /**
   * Maps the given function over the given list in parallel using this strategy.
   *
   * @param f  A function to map over the given list in parallel.
   * @param bs A list over which to map the given function in parallel.
   * @return A product-1 that returns the list with all of its elements transformed by the given function.
   */
  public <B> P1<List<A>> parMap(final F<B, A> f, final List<B> bs) {
    return sequence(bs.map(concurry(f)));
  }

  /**
   * Maps the given function over the given array in parallel using this strategy.
   *
   * @param f  A function to map over the given array in parallel.
   * @param bs An array over which to map the given function in parallel.
   * @return A product-1 that returns the array with all of its elements transformed by the given function.
   */
  public <B> P1<Array<A>> parMap(final F<B, A> f, final Array<B> bs) {
    return sequence(bs.map(concurry(f)));
  }

  /**
   * A strict version of parMap over lists.
   * Maps the given function over the given list in parallel using this strategy,
   * blocking the current thread until all values have been obtained.
   *
   * @param f  A function to map over the given list in parallel.
   * @param bs A list over which to map the given function in parallel.
   * @return A list with all of its elements transformed by the given function.
   */
  public <B> List<A> parMap1(final F<B, A> f, final List<B> bs) {
    return compose(P1.<List<A>>__1(), parMapList(f)).f(bs);
  }

  /**
   * A strict version of parMap over arrays.
   * Maps the given function over the given arrays in parallel using this strategy,
   * blocking the current thread until all values have been obtained.
   *
   * @param f  A function to map over the given array in parallel.
   * @param bs An array over which to map the given function in parallel.
   * @return An array with all of its elements transformed by the given function.
   */
  public <B> Array<A> parMap1(final F<B, A> f, final Array<B> bs) {
    return compose(P1.<Array<A>>__1(), parMapArray(f)).f(bs);
  }

  /**
   * Promotes a function to a parallel function on lists using this strategy.
   *
   * @param f A function to transform into a parallel function on lists.
   * @return The function transformed into a parallel function on lists.
   */
  public <B> F<List<B>, P1<List<A>>> parMapList(final F<B, A> f) {
    return new F<List<B>, P1<List<A>>>() {
      public P1<List<A>> f(final List<B> as) {
        return parMap(f, as);
      }
    };
  }

  /**
   * First-class version of parMap on lists.
   *
   * @return A function that promotes another function to a parallel function on lists.
   */
  public <B> F<F<B, A>, F<List<B>, P1<List<A>>>> parMapList() {
    return new F<F<B, A>, F<List<B>, P1<List<A>>>>() {
      public F<List<B>, P1<List<A>>> f(final F<B, A> f) {
        return parMapList(f);
      }
    };
  }

  /**
   * First-class version of parMap1 on lists (parallel list functor).
   *
   * @return A function that promotes another function to a blocking parallel function on lists.
   */
  public <B> F<F<B, A>, F<List<B>, List<A>>> parMapList1() {
    return new F<F<B, A>, F<List<B>, List<A>>>() {
      public F<List<B>, List<A>> f(final F<B, A> f) {
        return new F<List<B>, List<A>>() {
          public List<A> f(final List<B> bs) {
            return parMap1(f, bs);
          }
        };
      }
    };
  }

  /**
   * Promotes a function to a parallel function on arrays using this strategy.
   *
   * @param f A function to transform into a parallel function on arrays.
   * @return The function transformed into a parallel function on arrays.
   */
  public <B> F<Array<B>, P1<Array<A>>> parMapArray(final F<B, A> f) {
    return new F<Array<B>, P1<Array<A>>>() {
      public P1<Array<A>> f(final Array<B> as) {
        return parMap(f, as);
      }
    };
  }

  /**
   * First-class version of parMap on arrays.
   *
   * @return A function that promotes another function to a parallel function on arrays.
   */
  public <B> F<F<B, A>, F<Array<B>, P1<Array<A>>>> parMapArray() {
    return new F<F<B, A>, F<Array<B>, P1<Array<A>>>>() {
      public F<Array<B>, P1<Array<A>>> f(final F<B, A> f) {
        return parMapArray(f);
      }
    };
  }

  /**
   * First-class version of parMap1 on arrays (parallel array functor).
   *
   * @return A function that promotes another function to a blocking parallel function on arrays.
   */
  public <B> F<F<B, A>, F<Array<B>, Array<A>>> parMapArray1() {
    return new F<F<B, A>, F<Array<B>, Array<A>>>() {
      public F<Array<B>, Array<A>> f(final F<B, A> f) {
        return new F<Array<B>, Array<A>>() {
          public Array<A> f(final Array<B> bs) {
            return parMap1(f, bs);
          }
        };
      }
    };
  }

  /**
   * Binds the given function in parallel across the given list, using the given strategy, with a final join.
   *
   * @param s  The strategy to use for parallelization.
   * @param f  The function to bind across the given list.
   * @param as The list across which to bind the given function.
   * @return A P1 containing the result of the parallel map operation after the final join.
   */
  public static <A, B> P1<List<B>> parFlatMap(final Strategy<List<B>> s,
                                              final F<A, List<B>> f,
                                              final List<A> as) {
    return fmap(List.<B>join()).f(s.parMap(f, as));
  }

  /**
   * Binds the given function in parallel across the given array, using the given strategy, with a final join.
   *
   * @param s  The strategy to use for parallelization.
   * @param f  The function to bind across the given array.
   * @param as The array across which to bind the given function.
   * @return A P1 containing the result of the parallel map operation after the final join.
   */
  public static <A, B> P1<Array<B>> parFlatMap(final Strategy<Array<B>> s,
                                               final F<A, Array<B>> f,
                                               final Array<A> as) {
    return fmap(Array.<B>join()).f(s.parMap(f, as));
  }

  /**
   * Sequentially evaluates chunks (sub-sequences) of a list in parallel. Splits the list into chunks,
   * evaluating the chunks simultaneously, but each chunk as a sequence.
   *
   * @param s           The strategy to use for parallelization.
   * @param chunkLength The length of each sequence.
   * @param as          The list to evaluate in parallel chunks.
   * @return A product-1 containing the list of results extracted from the given list of product-1s.
   */
  public static <A> P1<List<A>> parListChunk(final Strategy<List<A>> s,
                                             final int chunkLength,
                                             final List<P1<A>> as) {
    return fmap(List.<A>join()).f(s.parList(as.partition(chunkLength).map(P1.<A>sequenceList())));
  }

  /**
   * Zips together two lists in parallel using a given function, with this strategy.
   * Calls the given function once for each corresponding pair in the lists, position-wise,
   * passing elements from the first list to the first argument of the function, and elements from the second list
   * to the second argument of the function, yielding a list of the results.
   * If the lists are not of the same length, the remaining elements of the longer list are ignored.
   *
   * @param f  The function of arity-2 with which to zip.
   * @param bs A list to zip with the given function.
   * @param cs A list to zip with the given function.
   * @return The list of the results of calling the given function on corresponding elements of the given lists.
   */
  public <B, C> P1<List<A>> parZipWith(final F2<B, C, A> f, final List<B> bs, final List<C> cs) {
    return sequence(bs.zipWith(cs, concurry(f)));
  }

  /**
   * Zips together two arrays in parallel using a given function, with this strategy.
   * Calls the given function once for each corresponding pair in the arrays, position-wise,
   * passing elements from the first array to the first argument of the function, and elements from the second array
   * to the second argument of the function, yielding a array of the results.
   * If the arrays are not of the same length, the remaining elements of the longer array are ignored.
   *
   * @param f  The function of arity-2 with which to zip.
   * @param bs A array to zip with the given function.
   * @param cs A array to zip with the given function.
   * @return The array of the results of calling the given function on corresponding elements of the given arrays.
   */
  public <B, C> P1<Array<A>> parZipWith(final F2<B, C, A> f, final Array<B> bs, final Array<C> cs) {
    return sequence(bs.zipWith(cs, concurry(f)));
  }

  /**
   * Lifts a given function of arity-2 so that it zips together two lists in parallel,
   * using this strategy, calling the function once for each corresponding pair in the lists, position-wise.
   *
   * @param f The function of arity-2 with which to zip.
   * @return A transformation that zips two lists using the argument function, in parallel.
   */
  public <B, C> F2<List<B>, List<C>, P1<List<A>>> parZipListWith(final F2<B, C, A> f) {
    return new F2<List<B>, List<C>, P1<List<A>>>() {
      public P1<List<A>> f(final List<B> bs, final List<C> cs) {
        return parZipWith(f, bs, cs);
      }
    };
  }

  /**
   * Lifts a given function of arity-2 so that it zips together two arrays in parallel,
   * using this strategy, calling the function once for each corresponding pair in the arrays, position-wise.
   *
   * @param f The function of arity-2 with which to zip.
   * @return A transformation that zips two arrays using the argument function, in parallel.
   */
  public <B, C> F2<Array<B>, Array<C>, P1<Array<A>>> parZipArrayWith(final F2<B, C, A> f) {
    return new F2<Array<B>, Array<C>, P1<Array<A>>>() {
      public P1<Array<A>> f(final Array<B> bs, final Array<C> cs) {
        return parZipWith(f, bs, cs);
      }
    };
  }

  /**
   * Returns a function which returns a product-1 which waits for the given Future to obtain a value.
   *
   * @return A function which, given a Future, yields a product-1 that waits for it.
   */
  public static <A> F<Future<A>, P1<A>> obtain() {
    return new F<Future<A>, P1<A>>() {
      public P1<A> f(final Future<A> t) {
        return obtain(t);
      }
    };
  }

  /**
   * Provides a product-1 that waits for the given future to obtain a value.
   *
   * @param t A Future for which to wait.
   * @return A product-1 that waits for the given future to obtain a value.
   */
  public static <A> P1<A> obtain(final Future<A> t) {
    return new P1<A>() {
      public A _1() {
        try {
          return t.get();
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();
          throw new Error(e);
        } catch (ExecutionException e) {
          throw new Error(e);
        }
      }
    };
  }

  /**
   * Returns an Effect that waits for a given Future to obtain a value, discarding the value.
   *
   * @return An effect, which, given a Future, waits for it to obtain a value, discarding the value.
   */
  public static <A> Effect<Future<A>> discard() {
    return new Effect<Future<A>>() {
      public void e(final Future<A> a) {
        Strategy.<A>obtain().f(a)._1();
      }
    };
  }

  /**
   * Provides a simple parallelization strategy that creates, and discards, a new thread for
   * every evaluation.
   *
   * @return a simple parallelization strategy that creates, and discards, a new thread for
   *         every evaluation.
   */
  public static <A> Strategy<A> simpleThreadStrategy() {
    return strategy(new F<P1<A>, P1<A>>() {
      public P1<A> f(final P1<A> p) {
        final FutureTask<A> t = new FutureTask<A>(Java.<A>P1_Callable().f(p));
        new Thread(t).start();
        return obtain(t);
      }
    });
  }

  /**
   * Provides a parallelization strategy that uses an ExecutorService to control the method and
   * degree of parallelism.
   *
   * @param s The ExecutorService to use for scheduling evaluations.
   * @return A Strategy that uses the provided ExecutorService to control the method and degree
   *         of parallelism.
   */
  public static <A> Strategy<A> executorStrategy(final ExecutorService s) {
    return strategy(new F<P1<A>, P1<A>>() {
      public P1<A> f(final P1<A> p) {
        return obtain(s.submit(Java.<A>P1_Callable().f(p)));
      }
    });
  }

  /**
   * Provides a parallelization strategy that uses a CompletionService to control the method and
   * degree of parallelism, and where each parallel task's completion is registered with the service.
   *
   * @param s The CompletionService to use for scheduling evaluations and detect their completion.
   * @return A Strategy that uses the provided CompletionService to control the method and degree of parallelism,
   *         and notifies the service of task completion.
   */
  public static <A> Strategy<A> completionStrategy(final CompletionService<A> s) {
    return strategy(new F<P1<A>, P1<A>>() {
      public P1<A> f(final P1<A> p) {
        return obtain(s.submit(Java.<A>P1_Callable().f(p)));
      }
    });
  }

  /**
   * Provides a strategy that performs sequential (non-concurrent) evaluation of its argument.
   *
   * @return A strategy that performs sequential (non-concurrent) evaluation of its argument.
   */
  public static <A> Strategy<A> seqStrategy() {
    return strategy(new F<P1<A>, P1<A>>() {
      public P1<A> f(final P1<A> a) {
        return P.p(a._1());
      }
    });
  }

  /**
   * Provides a strategy that performs no evaluation of its argument.
   *
   * @return A strategy that performs no evaluation of its argument.
   */
  public static <A> Strategy<A> idStrategy() {
    return strategy(Function.<P1<A>>identity());
  }

  /**
   * Maps the given bijective transformation across this strategy (Exponential Functor pattern).
   *
   * @param f A transformation from this strategy's codomain to the resulting strategy's codomain.
   * @param g A transformation from the resulting strategy's domain to this strategy's domain.
   * @return A new strategy that maps to this strategy and back again.
   */
  public <B> Strategy<B> xmap(final F<P1<A>, P1<B>> f, final F<P1<B>, P1<A>> g) {
    return strategy(compose(f, compose(f(), g)));
  }

  /**
   * Maps the given transformation across this strategy's domain (Invariant Functor pattern).
   *
   * @param f A transformation from this strategy's codomain to the resulting strategy's codomain.
   * @return A new strategy that applies the given transformation after each application of this strategy.
   */
  public Strategy<A> map(final F<P1<A>, P1<A>> f) {
    return xmap(f, Function.<P1<A>>identity());
  }

  /**
   * Maps the given transformation across this strategy's codomain (Invariant Functor pattern).
   *
   * @param f A transformation from the resulting strategy's domain to this strategy's domain.
   * @return A new strategy that applies the given transformation before each application of this strategy.
   */
  public Strategy<A> comap(final F<P1<A>, P1<A>> f) {
    return xmap(Function.<P1<A>>identity(), f);
  }

  /**
   * Provides an error-handling strategy. Captures any uncaught runtime errors encountered by the given strategy
   * and applies the given side-effect to them.
   *
   * @param s The strategy to equip with an error-handling effect.
   * @param e The effect that should handle errors.
   * @return A strategy that captures any runtime errors with a side-effect.
   */
  public static <A> Strategy<A> errorStrategy(final Strategy<A> s, final Effect<Error> e) {
    return s.comap(new F<P1<A>, P1<A>>() {
      public P1<A> f(final P1<A> a) {
        return new P1<A>() {
          public A _1() {
            try {
              return a._1();
            } catch (Throwable t) {
              final Error error = new Error(t);
              e.e(error);
              throw error;
            }
          }
        };
      }
    });
  }

  /**
   * Provides a normalising strategy that fully evaluates its Callable argument.
   *
   * @param s A non-normalising strategy to use for the evaluation.
   * @return A new strategy that fully evaluates Callables, using the given strategy.
   */
  public static <A> Strategy<Callable<A>> callableStrategy(final Strategy<Callable<A>> s) {
    return s.comap(new F<P1<Callable<A>>, P1<Callable<A>>>() {
      public P1<Callable<A>> f(final P1<Callable<A>> a) {
        return P1.curry(Callables.<A>normalise()).f(a._1());
      }
    });
  }

}
