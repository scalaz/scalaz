package fj.data.vector;

import fj.F;
import fj.F2;
import static fj.Function.curry;
import static fj.P.p2;
import fj.P1;
import fj.P2;
import fj.data.Array;
import fj.data.List;
import fj.data.NonEmptyList;
import fj.data.Stream;

import java.util.Iterator;

/**
 * A vector-2.
 */
public final class V2<A> implements Iterable<A> {

  private final P2<A, A> inner;

  private V2(final P2<A, A> inner) {
    this.inner = inner;
  }

  /**
   * Creates a vector-2 from a homogeneous product-2.
   *
   * @param p The product-2 from which to create a vector.
   * @return A new vector-2.
   */
  public static <A> V2<A> p(final P2<A, A> p) {
    return new V2<A>(p);
  }

  /**
   * Returns the first element of this vector.
   *
   * @return the first element of this vector.
   */
  public A _1() {
    return inner._1();
  }

  /**
   * Returns the second element of this vector.
   *
   * @return the second element of this vector.
   */
  public A _2() {
    return inner._2();
  }

  /**
   * Returns an iterator for the elements of this vector.
   *
   * @return an iterator for the elements of this vector.
   */
  public Iterator<A> iterator() {
    return toStream().iterator();
  }

  /**
   * Returns a homogeneous product-2 equivalent to this vector.
   *
   * @return a homogeneous product-2 equivalent to this vector.
   */
  public P2<A, A> p() {
    return inner;
  }

  /**
   * Returns a nonempty list with the elements of this vector.
   *
   * @return a nonempty list with the elements of this vector.
   */
  public NonEmptyList<A> toNonEmptyList() {
    return NonEmptyList.nel(_1(), List.single(_2()));
  }

  /**
   * Returns a stream of the elements of this vector.
   *
   * @return a stream of the elements of this vector.
   */
  public Stream<A> toStream() {
    return Stream.cons(_1(), new P1<Stream<A>>() {
      public Stream<A> _1() {
        return Stream.single(_2());
      }
    });
  }

  /**
   * Returns a function that transforms a vector-2 to a stream of its elements.
   *
   * @return a function that transforms a vector-2 to a stream of its elements.
   */
  public static <A> F<V2<A>, Stream<A>> toStream_() {
    return new F<V2<A>, Stream<A>>() {
      public Stream<A> f(final V2<A> v) {
        return v.toStream();
      }
    };
  }

  /**
   * Returns a function that transforms a vector-2 to the equivalent product-2.
   *
   * @return a function that transforms a vector-2 to the equivalent product-2.
   */
  public static <A> F<V2<A>, P2<A, A>> p_() {
    return new F<V2<A>, P2<A, A>>() {
      public P2<A, A> f(final V2<A> v) {
        return v.p();
      }
    };
  }

  /**
   * Returns an array with the elements of this vector.
   *
   * @return an array with the elements of this vector.
   */
  @SuppressWarnings("unchecked")
  public Array<A> toArray() {
    return Array.array(_1(), _2());
  }

  /**
   * Maps the given function across this vector.
   *
   * @param f The function to map across this vector.
   * @return A new vector after the given function has been applied to each element.
   */
  public <B> V2<B> map(final F<A, B> f) {
    return p(inner.split(f, f));
  }

  /**
   * Performs function application within a vector (applicative functor pattern).
   *
   * @param vf The vector of functions to apply.
   * @return A new vector after zipping the given vector of functions over this vector.
   */
  public <B> V2<B> apply(final V2<F<A, B>> vf) {
    return p(inner.split(vf._1(), vf._2()));
  }

  /**
   * Zips this vector with the given vector using the given function to produce a new vector.
   *
   * @param bs The vector to zip this vector with.
   * @param f  The function to zip this vector and the given vector with.
   * @return A new vector with the results of the function.
   */
  public <B, C> V2<C> zipWith(final F<A, F<B, C>> f, final V2<B> bs) {
    return bs.apply(map(f));
  }

  /**
   * Zips this vector with the given vector to produce a vector of pairs.
   *
   * @param bs The vector to zip this vector with.
   * @return A new vector with a length the same as the shortest of this vector and the given
   *         vector.
   */
  public <B> V2<P2<A, B>> zip(final V2<B> bs) {
    final F<A, F<B, P2<A, B>>> __2 = p2();
    return zipWith(__2, bs);
  }

  /**
   * Zips this vector with the given vector to produce a vector of vectors.
   *
   * @param bs The vector to zip this vector with.
   * @return A new vector of vectors.
   */
  public V2<V2<A>> vzip(final V2<A> bs) {
    final F2<A, A, V2<A>> __2 = V.v2();
    return zipWith(curry(__2), bs);
  }

  /**
   * Return the first element of this vector as a product-1.
   *
   * @return the first element of this vector as a product-1.
   */
  public P1<A> head() {
    return new P1<A>() {
      public A _1() {
        return V2.this._1();
      }
    };
  }

}
