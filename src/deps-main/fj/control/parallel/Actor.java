package fj.control.parallel;

import fj.Effect;
import fj.F;
import fj.Unit;
import fj.P1;

/**
 * Light weight actors for Java. Concurrency is controlled by a parallel Strategy.
 * The Strategy serves as the Actor's execution engine, and as its mailbox.
 * <p/>
 * Given some effect, the Actor performs the effect on its messages using its Strategy, transforming them
 * into unit-products. The unit-product represents a possibly running computation which is executing the effect.
 * <p/>
 * Author: Runar
 */
public final class Actor<A> {

  private final Strategy<Unit> s;
  private final F<A, P1<Unit>> f;

  private Actor(final Strategy<Unit> s, final F<A, P1<Unit>> e) {
    this.s = s;
    f = new F<A, P1<Unit>>() {
      public P1<Unit> f(final A a) {
        return s.par(e.f(a));
      }
    };
  }


  /**
   * Creates a new Actor that uses the given parallelization strategy and has the given side-effect.
   *
   * @param s The parallelization strategy to use for the new Actor.
   * @param e The side-effect to apply to messages passed to the Actor.
   * @return A new actor that uses the given parallelization strategy and has the given side-effect.
   */
  public static <A> Actor<A> actor(final Strategy<Unit> s, final Effect<A> e) {
    return new Actor<A>(s, P1.curry(Effect.Projection.e(e)));
  }

  /**
   * Creates a new Actor that uses the given parallelization strategy and has the given side-effect.
   *
   * @param s The parallelization strategy to use for the new Actor.
   * @param e The function projection of a side-effect to apply to messages passed to the Actor.
   * @return A new actor that uses the given parallelization strategy and has the given side-effect.
   */
  public static <A> Actor<A> actor(final Strategy<Unit> s, final F<A, P1<Unit>> e) {
    return new Actor<A>(s, e);
  }

  /**
   * Pass a message to this actor, applying its side-effect to the message. The side-effect is applied in a concurrent
   * computation, resulting in a product referencing that computation.
   *
   * @param a The message to send to this actor.
   * @return A unit-product that represents the action running concurrently.
   */
  public P1<Unit> act(final A a) {
    return f.f(a);
  }

  /**
   * Contravariant functor pattern. Creates a new actor whose message is transformed by the given function
   * before being passed to this actor.
   *
   * @param f The function to use for the transformation
   * @return A new actor which passes its messages through the given function, to this actor.
   */
  public <B> Actor<B> comap(final F<B, A> f) {
    return actor(s, new F<B, P1<Unit>>() {
      public P1<Unit> f(final B b) {
        return act(f.f(b));
      }
    });
  }

  /**
   * Transforms this actor to an actor on promises.
   *
   * @return A new actor, equivalent to this actor, that acts on promises.
   */
  public Actor<Promise<A>> promise() {
    return actor(s, new Effect<Promise<A>>() {
      public void e(final Promise<A> b) {
        b.to(Actor.this);
      }
    });
  }

}
