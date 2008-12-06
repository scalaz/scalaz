package fj.data.hlist;

import fj.F;
import fj.F2;
import fj.F3;
import fj.P;
import fj.P2;
import fj.Unit;
import static fj.Function.compose;

/**
 * Type-safe heterogeneous lists.
 *
 * @param <A> The specific type of the list, as a subtype of HList
 */
public abstract class HList<A extends HList<A>> {

  protected HList() {
  }

  /**
   * Extends (cons) this list by prepending the given element, returning a new list.
   *
   * @param e an element to prepend to this list.
   * @return a new heterogeneous list, consisting of the given element prepended to this list.
   */
  public abstract <E> HCons<E, A> extend(E e);

  public abstract <E> Apply<Unit, P2<E, A>, HCons<E, A>> extender();

  private static final HNil nil = new HNil();

  /**
   * Returns the empty list.
   *
   * @return the empty list.
   */
  public static HNil nil() {
    return nil;
  }

  /**
   * Returns a heterogeneous list consisting of an element and another list.
   *
   * @param e an element to put in a list.
   * @param l the rest of the list.
   * @return a heterogeneous list consisting of an element and another list.
   */
  public static <E, L extends HList<L>> HCons<E, L> cons(final E e, final L l) {
    return new HCons<E, L>(e, l);
  }

  /**
   * Returns a heterogeneous list consisting of a single element.
   *
   * @param e an element to put in a list
   * @return a heterogeneous list consisting of a single element.
   */
  public static <E> HCons<E, HNil> single(final E e) {
    return cons(e, nil());
  }

  /**
   * The concatenation of two heterogeneous lists.
   *
   * @param <A> The type of the first list.
   * @param <B> The type of the second list.
   * @param <C> The type of the combined list.
   */
  public static final class HAppend<A, B, C> {
    private final F2<A, B, C> append;

    private HAppend(final F2<A, B, C> f) {
      append = f;
    }

    /**
     * Append a given heterogeneous list to another.
     *
     * @param a a heterogeneous list to be appended to.
     * @param b a heterogeneous list to append to another.
     * @return a new heterogeneous list consisting of the second argument appended to the first.
     */
    public C append(final A a, final B b) {
      return append.f(a, b);
    }

    /**
     * Returns a method for concatenating lists to the empty list.
     *
     * @return a method for concatenating lists to the empty list.
     */
    public static <L extends HList<L>> HAppend<HNil, L, L> append() {
      return new HAppend<HNil, L, L>(new F2<HNil, L, L>() {
        public L f(final HNil hNil, final L l) {
          return l;
        }
      });
    }

    /**
     * Returns a method for appending lists to a nonempty heterogeneous list.
     *
     * @param h a method for appending lists to the tail of the given nonempty list.
     * @return a method for appending lists to a nonempty heterogeneous list.
     */
    public static <X, A extends HList<A>, B, C extends HList<C>, H extends HAppend<A, B, C>>
    HAppend<HCons<X, A>, B, HCons<X, C>> append(final H h) {
      return new HAppend<HCons<X, A>, B, HCons<X, C>>(new F2<HCons<X, A>, B, HCons<X, C>>() {
        public HCons<X, C> f(final HCons<X, A> c, final B l) {
          return cons(c.head(), h.append(c.tail(), l));
        }
      });
    }
  }

  /**
   * Type-level function application operators.
   *
   * @param <F$> The type of the function to apply.
   * @param <A> The domain of the function.
   * @param <R> The function's codomain.
   */
  public abstract static class Apply<F$, A, R> {
    public abstract R apply(F$ f, A a);

    /**
     * Function application operator.
     *
     * @return an operator that applies a given function to a given argument.
     */
    public static <X, Y> Apply<F<X, Y>, X, Y> f() {
      return new Apply<F<X, Y>, X, Y>() {
        public Y apply(final F<X, Y> f, final X x) {
          return f.f(x);
        }
      };
    }

    /**
     * Identity operator
     *
     * @return An operator that returns its second argument no matter which function is being applied.
     */
    public static <X> Apply<Unit, X, X> id() {
      return new Apply<Unit, X, X>() {
        public X apply(final Unit f, final X x) {
          return x;
        }
      };
    }

    /**
     * A function application operator for function composition.
     *
     * @param <X> The domain.
     * @param <Y> The type through which to compose.
     * @param <Z> The codomain.
     * @return an operator that composes functions.
     */
    public static <X, Y, Z> Apply<Unit, P2<F<X, Y>, F<Y, Z>>, F<X, Z>> comp() {
      return new Apply<Unit, P2<F<X, Y>, F<Y, Z>>, F<X, Z>>() {
        public F<X, Z> apply(final Unit f, final P2<F<X, Y>, F<Y, Z>> fs) {
          return compose(fs._2(), fs._1());
        }
      };
    }

    /**
     * An operator for the construction of heterogeneous lists.
     *
     * @return an operator that constructs heterogeneous lists.
     */
    public static <E, L extends HList<L>> Apply<Unit, P2<E, L>, HCons<E, L>> cons() {
      return new Apply<Unit, P2<E, L>, HCons<E, L>>() {
        public HCons<E, L> apply(final Unit f, final P2<E, L> p) {
          return HList.cons(p._1(), p._2());
        }
      };
    }

    /**
     * A function application operator for concatenating heterogeneous lists.
     * @param <A> The type of the list to which to append.
     * @param <B> The type of the list to append.
     * @param <C> The type of the concatenated list. 
     * @return an operator that concatenates heterogeneous lists.
     */
    public static <A, B, C> Apply<HAppend<A, B, C>, P2<A, B>, C> append() {
      return new Apply<HAppend<A, B, C>, P2<A, B>, C>() {
        public C apply(final HAppend<A, B, C> f, final P2<A, B> p) {
          return f.append(p._1(), p._2());
        }
      };
    }
  }

  /**
   * The catamorphism over heterogeneous lists.
   *
   * @param <G> The type of the function with which to fold.
   * @param <V> The type of the value to be substituted for the empty list.
   * @param <L> The type of the heterogeneous list to be folded.
   * @param <R> The return type of the fold.
   */
  public static class HFoldr<G, V, L, R> {

    private final F3<G, V, L, R> foldRight;

    private HFoldr(final F3<G, V, L, R> foldRight) {
      this.foldRight = foldRight;
    }

    /**
     * A fold instance for the empty list.
     *
     * @param <G> The type of the function with which to fold.
     * @param <V>  The type of value that this fold returns.
     * @return a fold instance for the empty list.
     */
    public static <G, V> HFoldr<G, V, HNil, V> hFoldr() {
      return new HFoldr<G, V, HNil, V>(new F3<G, V, HNil, V>() {
        public V f(final G f, final V v, final HNil hNil) {
          return v;
        }
      });
    }

    /**
     * A fold instance for a non-empty heterogeneous list
     *
     * @param p    An operator that applies a function on the head of the list and the fold of its tail.
     * @param h    A fold instance for the tail of the list.
     * @param <E>  The type of the head of the list.
     * @param <G>  The type of function to apply to the head of the list and the fold of its tail.
     * @param <V>  The type of value to substitute for the empty list.
     * @param <L>  The type of the tail of the list.
     * @param <R>  The type of the fold of the tail of the list.
     * @param <RR> The return type of the fold.
     * @param <H>  The type of the fold instance for the tail of the list.
     * @param <PP> The type of the given function application operator.
     * @return A fold instance for a non-empty heterogeneous list.
     */
    public static <E, G, V, L extends HList<L>, R, RR,
        H extends HFoldr<G, V, L, R>,
        PP extends Apply<G, P2<E, R>, RR>>
    HFoldr<G, V, HCons<E, L>, RR> hFoldr(final PP p, final H h) {
      return new HFoldr<G, V, HCons<E, L>, RR>(new F3<G, V, HCons<E, L>, RR>() {
        public RR f(final G f, final V v, final HCons<E, L> c) {
          return p.apply(f, P.p(c.head(), h.foldRight(f, v, c.tail())));
        }
      });
    }

    /**
     * Folds a non-empty heterogeneous list.
     *
     * @param f A function with which to fold.
     * @param v The value to substitute for the empty list.
     * @param l The heterogeneous list to be folded.
     * @return a value obtained by folding the given list with the given function.
     */
    public R foldRight(final G f, final V v, final L l) {
      return foldRight.f(f, v, l);
    }

  }

  /**
 * The nonempty list
   */
  public static final class HCons<E, L extends HList<L>> extends HList<HCons<E, L>> {
    private E e;
    private L l;

    HCons(final E e, final L l) {
      this.e = e;
      this.l = l;
    }

    public E head() {
      return e;
    }

    public L tail() {
      return l;
    }

    public <X> Apply<Unit, P2<X, HCons<E, L>>, HCons<X, HCons<E, L>>> extender() {
      return Apply.cons();
    }

    public <X> HCons<X, HCons<E, L>> extend(final X e) {
      return cons(e, this);
    }

  }

  /**
   * The empty list
   */
  public static final class HNil extends HList<HNil> {
    HNil() {
    }

    public <E> HCons<E, HNil> extend(final E e) {
      return cons(e, this);
    }

    public <E> Apply<Unit, P2<E, HNil>, HCons<E, HNil>> extender() {
      return Apply.cons();
    }

  }
}
