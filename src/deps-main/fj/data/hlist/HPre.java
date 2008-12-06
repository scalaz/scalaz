package fj.data.hlist;

import fj.F;
import fj.pre.Show;

/**
 * A basic prelude of values lifted into the type system.
 */
@SuppressWarnings({"ALL"})
public final class HPre {
  private HPre() {
    throw new UnsupportedOperationException();
  }

  /**
   * A type-level Boolean
   */
  public static class HBool {
    private HBool() {
    }
  }

  /**
   * Boolean true
   */
  public static class HTrue extends HBool {
    private HTrue() {
    }
  }

  /**
   * Boolean false
   */
  public static class HFalse extends HBool {
    private HFalse() {
    }
  }

  private static final HTrue hTrue = new HTrue();
  private static final HFalse hFalse = new HFalse();

  /**
   * Returns a boolean value whose type represents truth.
   *
   * @return a boolean value whose type represents truth.
   */
  public static HTrue hTrue() {
    return hTrue;
  }

  /**
   * Returns a boolean value whose type represents falsehood.
   *
   * @return a boolean value whose type represents falsehood.
   */
  public static HFalse hFalse() {
    return hFalse;
  }

  /**
   * Type-level boolean conjunction. A value of this type represents evidence that AB -> C
   *
   * @param <A> A boolean
   * @param <B> A boolean
   * @param <C> The logical implication of A and B
   */
  public static final class HAnd<A extends HBool, B extends HBool, C extends HBool> {
    private final C v;

    private HAnd(final C v) {
      this.v = v;
    }

    public C v() {
      return v;
    }

    public static HAnd<HFalse, HFalse, HFalse> hAnd(final HFalse a, final HFalse b) {
      return new HAnd<HFalse, HFalse, HFalse>(hFalse());
    }

    public static HAnd<HTrue, HFalse, HFalse> hAnd(final HTrue a, final HFalse b) {
      return new HAnd<HTrue, HFalse, HFalse>(hFalse());
    }

    public static HAnd<HFalse, HTrue, HFalse> hAnd(final HFalse a, final HTrue b) {
      return new HAnd<HFalse, HTrue, HFalse>(hFalse());
    }

    public static HAnd<HTrue, HTrue, HTrue> hAnd(final HTrue a, final HTrue b) {
      return new HAnd<HTrue, HTrue, HTrue>(hTrue());
    }
  }

  /**
   * Type-level boolean disjunction. A value of this type represents evidence that A+B -> C
   *
   * @param <A> A boolean
   * @param <B> A boolean
   * @param <C> The logical implication of A or B
   */
  public static final class HOr<A extends HBool, B extends HBool, C extends HBool> {
    private final C v;

    private HOr(final C v) {
      this.v = v;
    }

    public C v() {
      return v;
    }

    public static HOr<HFalse, HFalse, HFalse> hOr(final HFalse a, final HFalse b) {
      return new HOr<HFalse, HFalse, HFalse>(hFalse());
    }

    public static HOr<HTrue, HFalse, HTrue> hOr(final HTrue a, final HFalse b) {
      return new HOr<HTrue, HFalse, HTrue>(hTrue());
    }

    public static HOr<HFalse, HTrue, HTrue> hOr(final HFalse a, final HTrue b) {
      return new HOr<HFalse, HTrue, HTrue>(hTrue());
    }

    public static HOr<HTrue, HTrue, HTrue> hOr(final HTrue a, final HTrue b) {
      return new HOr<HTrue, HTrue, HTrue>(hTrue());
    }
  }

  /**
   * A type-level conditional. The type of the last parameter is implied by the first three.
   *
   * @param <T> A boolean
   * @param <X> The type of Z if T is true.
   * @param <Y> The type of Z if T is false.
   * @param <Z> A type that is either X or Z, depending on T.
   */
  public static final class HCond<T, X, Y, Z> {
    private HCond(final Z z) {
      this.z = z;
    }

    private final Z z;

    public Z v() {
      return z;
    }

    public static <X, Y> HCond<HFalse, X, Y, Y> hCond(final HFalse t, final X x, final Y y) {
      return new HCond<HFalse, X, Y, Y>(y);
    }

    public static <X, Y> HCond<HTrue, X, Y, X> hCond(final HTrue t, final X x, final Y y) {
      return new HCond<HTrue, X, Y, X>(x);
    }
  }

  /**
   * Type-level natural numbers.
   */
  public abstract static class HNat<A extends HNat<A>> {
    public abstract Show<A> show();

    public abstract Integer toInteger();

    public static HZero hZero() {
      return new HZero();
    }

    public static <N extends HNat<N>> HSucc<N> hSucc(final N n) {
      return new HSucc<N>(n);
    }

    public static <N extends HNat<N>> N hPred(final HSucc<N> n) {
      return n.pred;
    }
  }

  /**
   * Type-level zero
   */
  public static final class HZero extends HNat<HZero> {
    private HZero() {
    }

    public Show<HZero> show() {
      return Show.showS(new F<HZero, String>() {
        public String f(final HZero hZero) {
          return "HZero";
        }
      });
    }

    public Integer toInteger() {
      return 0;
    }
  }

  /**
   * A natural number N + 1
   *
   * @param <N> The predecessor of this number.
   */
  public static final class HSucc<N extends HNat<N>> extends HNat<HSucc<N>> {
    private HSucc(final N n) {
      pred = n;
    }

    private final N pred;

    public Show<HSucc<N>> show() {
      return Show.showS(new F<HSucc<N>, String>() {
        public String f(final HSucc<N> s) {
          return "HSucc (" + s.show().showS(s) + ')';
        }
      });
    }

    public Integer toInteger() {
      return 1 + pred.toInteger();
    }
  }

  /**
   * Type-level equality. Represents evidence for X and Y being equal, or counterevidence against.
   */
  public static final class HEq<X, Y, B extends HBool> {
    private final B v;

    private HEq(final B v) {
      this.v = v;
    }

    public B v() {
      return v;
    }

    /**
     * Zero is equal to itself.
     * @param a Zero
     * @param b Zero
     * @return Equality for Zero
     */
    public static HEq<HZero, HZero, HTrue> eq(final HZero a, final HZero b) {
      return new HEq<HZero, HZero, HTrue>(hTrue());
    }

    /**
     * Zero is not equal to anything other than zero.
     */
    public static <N extends HNat<N>> HEq<HZero, HSucc<N>, HFalse> eq(final HZero a, final HSucc<N> b) {
      return new HEq<HZero, HSucc<N>, HFalse>(hFalse());
    }

    /**
     * Zero is not equal to anything other than zero.
     */
    public static <N extends HNat<N>> HEq<HSucc<N>, HZero, HFalse> eq(final HSucc<N> a, final HZero b) {
      return new HEq<HSucc<N>, HZero, HFalse>(hFalse());
    }

    /**
     * A number is equal to another if their predecessors are equal.
     */
    public static <N extends HNat<N>, NN extends HNat<NN>, B extends HBool, E extends HEq<N, NN, B>>
    HEq<HSucc<N>, HSucc<NN>, B> eq(final HSucc<N> a, final HSucc<NN> b, final E e) {
      return new HEq<HSucc<N>, HSucc<NN>, B>(e.v);
    }

  }

  /**
   * Type-level integer arithmetic
   */
  public static final class HAdd<A extends HNat<A>, B extends HNat<B>, C extends HNat<C>> {
    private final C sum;
    private HAdd(final C sum) {
      this.sum = sum;
    }

    /**
     * The sum of zero and any other number is that number.
     */
    public static <N extends HNat<N>> HAdd<HZero, HSucc<N>, HSucc<N>> add(final HZero a, final HSucc<N> b) {
      return new HAdd<HZero, HSucc<N>, HSucc<N>>(b);
    }

    /**
     * The sum of zero and any other number is that number.
     */
    public static <N extends HNat<N>> HAdd<HSucc<N>, HZero, HSucc<N>> add(final HSucc<N> a, final HZero b) {
      return new HAdd<HSucc<N>, HZero, HSucc<N>>(a);
    }

    /**
     * The sum of numbers a and b is one greater than the sum of b and the predecessor of a. 
     */
    public static <N extends HNat<N>, M extends HNat<M>, R extends HNat<R>, H extends HAdd<N, HSucc<M>, R>>
    HAdd<HSucc<N>, HSucc<M>, HSucc<R>> add(final HSucc<N> a, final HSucc<M> b, final H h) {
      return new HAdd<HSucc<N>, HSucc<M>, HSucc<R>>(HNat.hSucc(h.sum));
    }
  }

}
