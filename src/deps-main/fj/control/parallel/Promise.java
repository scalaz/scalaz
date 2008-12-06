package fj.control.parallel;

import fj.Effect;
import fj.F;
import fj.F2;
import fj.P;
import fj.P1;
import fj.P2;
import fj.Unit;
import static fj.Function.curry;
import static fj.Function.identity;
import static fj.control.parallel.Actor.actor;
import static fj.control.parallel.Callables.normalise;
import static fj.control.parallel.QueueActor.queueActor;
import fj.data.Either;
import fj.data.List;
import fj.data.Option;
import static fj.data.Option.none;
import static fj.data.Option.some;

import java.util.LinkedList;
import java.util.Queue;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * Represents a non-blocking future value. Products, functions, and actors, given to the methods on this class,
 * are executed concurrently, and the Promise serves as a handle on the result of the computation. Provides monadic
 * operations so that future computations can be combined
 * <p/>
 * Author: Runar
 */
public final class Promise<A> {

  private final Actor<P2<Either<P1<A>, Actor<A>>, Promise<A>>> actor;

  private final Strategy<Unit> s;

  private final CountDownLatch l = new CountDownLatch(1);
  private volatile Option<A> v = none();
  private final Queue<Actor<A>> waiting = new LinkedList<Actor<A>>();

  private Promise(final Strategy<Unit> s, final Actor<P2<Either<P1<A>, Actor<A>>, Promise<A>>> qa) {
    this.s = s;
    actor = qa;
  }

  private static <A> Promise<A> mkPromise(final Strategy<Unit> s) {
    final Actor<P2<Either<P1<A>, Actor<A>>, Promise<A>>> q =
            queueActor(s, new Effect<P2<Either<P1<A>, Actor<A>>, Promise<A>>>() {
              public void e(final P2<Either<P1<A>, Actor<A>>, Promise<A>> p) {
                final Promise<A> snd = p._2();
                final Queue<Actor<A>> as = snd.waiting;
                if (p._1().isLeft()) {
                  final A a = p._1().left().value()._1();
                  snd.v = some(a);
                  snd.l.countDown();
                  while (!as.isEmpty())
                    as.remove().act(a);
                } else if (snd.v.isNone())
                  as.add(p._1().right().value());
                else
                  p._1().right().value().act(snd.v.some());
              }
            }).asActor();
    return new Promise<A>(s, q);
  }

  /**
   * Promises to provide the value of the given 1-product, in the future.
   * Represents the unit function for promises.
   *
   * @param s The strategy with which to fulfil the promise.
   * @param a The 1-product to evaluate concurrently.
   * @return A promise representing the future result of evaluating the given 1-product.
   */
  public static <A> Promise<A> promise(final Strategy<Unit> s, final P1<A> a) {
    final Promise<A> p = mkPromise(s);
    p.actor.act(P.p(Either.<P1<A>, Actor<A>>left(a), p));
    return p;
  }

  /**
   * Provides a first-class unit function for promises.
   *
   * @param s The strategy with which to fulfil promises.
   * @return A function that, given a 1-product, yields a promise of that product's value.
   */
  public static <A> F<P1<A>, Promise<A>> promise(final Strategy<Unit> s) {
    return new F<P1<A>, Promise<A>>() {
      public Promise<A> f(final P1<A> a) {
        return promise(s, a);
      }
    };
  }

  /**
   * Provides a promise to call the given Callable in the future.
   *
   * @param s The strategy with which to fulfil the promise.
   * @param a The Callable to evaluate concurrently.
   * @return A promise of a new Callable that will return the result of calling the given Callable.
   */
  public static <A> Promise<Callable<A>> promise(final Strategy<Unit> s, final Callable<A> a) {
    return promise(s, new P1<Callable<A>>() {
      public Callable<A> _1() {
        return normalise(a);
      }
    });
  }

  /**
   * Transforms any function so that it returns a promise of a value instead of an actual value.
   * Represents the Kleisli arrow for the Promise monad.
   *
   * @param s The strategy with which to fulfil the promise.
   * @param f The function to turn into a promise-valued function.
   * @return The given function transformed into a function that returns a promise.
   */
  public static <A, B> F<A, Promise<B>> promise(final Strategy<Unit> s, final F<A, B> f) {
    return new F<A, Promise<B>>() {
      public Promise<B> f(final A a) {
        return promise(s, P1.curry(f).f(a));
      }
    };
  }

  /**
   * Promises to send a value to the given actor in the future.
   *
   * @param a An actor that will receive this Promise's value in the future.
   */
  public void to(final Actor<A> a) {
    actor.act(P.p(Either.<P1<A>, Actor<A>>right(a), this));
  }

  /**
   * Provides a promise to apply the given function to this promise's future value (covariant functor pattern).
   *
   * @param f The function to apply to this promise's future value.
   * @return A promise representing the future result of applying the given function to this promised value.
   */
  public <B> Promise<B> fmap(final F<A, B> f) {
    return bind(promise(s, f));
  }

  /**
   * Promotes any function to a transformation between promises (covariant functor pattern).
   *
   * @param f The function to promote to a transformation between promises.
   * @return That function lifted to a function on Promises.
   */
  public static <A, B> F<Promise<A>, Promise<B>> fmap_(final F<A, B> f) {
    return new F<Promise<A>, Promise<B>>() {
      public Promise<B> f(final Promise<A> a) {
        return a.fmap(f);
      }
    };
  }

  /**
   * Turns a promise of a promise into just a promise. The join function for the Promise monad.
   * Promise to give it a Promise of an A, and it will promise you an A in return.
   *
   * @param p A promise of a promise.
   * @return The promised promise.
   */
  public static <A> Promise<A> join(final Promise<Promise<A>> p) {
    final F<Promise<A>, Promise<A>> id = identity();
    return p.bind(id);
  }

  /**
   * Turns a product of a promise into just a promise. Does not block on the product by calling it,
   * but creates a new promise with a final join.
   *
   * @param s The strategy with which to fulfil the promise.
   * @param p A product-1 of a promise to turn into just a promise.
   * @return The joined promise.
   */
  public static <A> Promise<A> join(final Strategy<Unit> s, final P1<Promise<A>> p) {
    return join(promise(s, p));
  }

  /**
   * Binds the given function over this promise, with a final join.
   * The bind function for the Promise monad.
   *
   * @param f The function to bind over this promise.
   * @return The result of applying the given function to this promised value.
   */
  public <B> Promise<B> bind(final F<A, Promise<B>> f) {
    final Promise<B> r = mkPromise(s);
    final Actor<B> ab = actor(s, new Effect<B>() {
      public void e(final B b) {
        r.actor.act(P.p(Either.<P1<B>, Actor<B>>left(P.p(b)), r));
      }
    });
    to(ab.promise().comap(f));
    return r;
  }

  /**
   * Performs function application within a promise (applicative functor pattern).
   *
   * @param pf The promised function to apply.
   * @return A new promise after applying the given promised function to this promise.
   */
  public <B> Promise<B> apply(final Promise<F<A, B>> pf) {
    return pf.bind(new F<F<A, B>, Promise<B>>() {
      public Promise<B> f(final F<A, B> f) {
        return fmap(f);
      }
    });
  }

  /**
   * Binds the given function to this promise and the given promise, with a final join.
   *
   * @param pb A promise with which to bind the given function.
   * @param f  The function to apply to the given promised values.
   * @return A new promise after performing the map, then final join.
   */
  public <B, C> Promise<C> bind(final Promise<B> pb, final F<A, F<B, C>> f) {
    return pb.apply(fmap(f));
  }

  /**
   * Binds the given function to this promise and the given promise, with a final join.
   *
   * @param p A promise with which to bind the given function.
   * @param f  The function to apply to the given promised values.
   * @return A new promise after performing the map, then final join.
   */
  public <B, C> Promise<C> bind(final P1<Promise<B>> p, final F<A, F<B, C>> f) {
    return Promise.join(s, p).apply(fmap(f));
  }

  /**
   * Promotes a function of arity-2 to a function on promises.
   *
   * @param f The function to promote.
   * @return A function of arity-2 promoted to map over promises.
   */
  public static <A, B, C> F<Promise<A>, F<Promise<B>, Promise<C>>> liftM2(final F<A, F<B, C>> f) {
    return curry(new F2<Promise<A>, Promise<B>, Promise<C>>() {
      public Promise<C> f(final Promise<A> ca, final Promise<B> cb) {
        return ca.bind(cb, f);
      }
    });
  }

  /**
   * Turns a List of promises into a single promise of a List.
   *
   * @param s  The strategy with which to sequence the promises.
   * @param as The list of promises to transform.
   * @return A single promise for the given List.
   */
  public static <A> Promise<List<A>> sequence(final Strategy<Unit> s, final List<Promise<A>> as) {
    return join(foldRight(s, liftM2(List.<A>cons()), promise(s, P.p(List.<A>nil()))).f(as));
  }

  /**
   * First-class version of the sequence function.
   *
   * @param s The strategy with which to sequence a given list of promises.
   * @return A function that turns a list of promises into a single promise of a list.
   */
  public static <A> F<List<Promise<A>>, Promise<List<A>>> sequence(final Strategy<Unit> s) {
    return new F<List<Promise<A>>, Promise<List<A>>>() {
      public Promise<List<A>> f(final List<Promise<A>> as) {
        return sequence(s, as);
      }
    };
  }

  /**
   * Performs a right-fold reduction across a list in constant stack space.
   *
   * @param s The strategy with which to fold the list.
   * @param f The function to apply on each element of the list.
   * @param b The beginning value to start the application from.
   * @return The final result after the right-fold reduction.
   */
  public static <A, B> F<List<A>, Promise<B>> foldRight(final Strategy<Unit> s, final F<A, F<B, B>> f, final B b) {
    return new F<List<A>, Promise<B>>() {
      public Promise<B> f(final List<A> as) {
        return as.isEmpty() ? promise(s, P.p(b)) : liftM2(f).f(promise(s, P.p(as.head()))).f(
                join(s, P1.curry(this).f(as.tail())));
      }
    };
  }

  /**
   * Waits if necessary for the computation to complete, and then retrieves its result.
   *
   * @return The promised value.
   * @throws InterruptedException if the current thread is interrupted while waiting.
   */
  public A claim() throws InterruptedException {
    l.await();
    return v.some();
  }

  /**
   * Waits if necessary for the computation to complete, and then retrieves its result.
   *
   * @param timeout the maximum time to wait
   * @param unit    the time unit of the timeout argument
   * @return The promised value.
   * @throws InterruptedException if the current thread is interrupted while waiting.
   */
  public A claim(final long timeout, final TimeUnit unit) throws InterruptedException {
    l.await(timeout, unit);
    return v.some();
  }

  /**
   * Returns true if this promise has been fulfilled.
   *
   * @return true if this promise has been fulfilled.
   */
  public boolean isFulfilled() {
    return v.isSome();
  }

}
