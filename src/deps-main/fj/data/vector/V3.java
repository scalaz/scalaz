package fj.data.vector;

import fj.F;
import fj.F2;
import fj.P1;
import fj.P2;
import fj.P3;
import static fj.Function.curry;
import static fj.P.p2;
import fj.data.Array;
import fj.data.NonEmptyList;
import fj.data.Stream;

import java.util.Iterator;

/**
 * A vector-3.
 */
public final class V3<A> implements Iterable<A> {

  private final V2<A> tail;
  private final P1<A> head;

  private V3(final P1<A> head, final V2<A> tail) {
    this.head = head;
    this.tail = tail;
  }

  /**
   * Creates a vector-3 from a homogeneous product-3.
   *
   * @param p The product-3 from which to create a vector.
   * @return A new vector-3.
   */
  public static <A> V3<A> p(final P3<A, A, A> p) {
    return new V3<A>(new P1<A>() {
      public A _1() {
        return p._1();
      }
    }, V2.p(new P2<A, A>() {
      public A _1() {
        return p._2();
      }

      public A _2() {
        return p._3();
      }
    }));
  }

  /**
   * Creates a vector-3 from a head and a tail.
   *
   * @param head The value to put as the first element of the vector.
   * @param tail The vector representing all but the first element of the new vector.
   * @return The new vector.
   */
  public static <A> V3<A> cons(final P1<A> head, final V2<A> tail) {
    return new V3<A>(head, tail);
  }

  /**
   * Returns the first element of this vector.
   *
   * @return the first element of this vector.
   */
  public A _1() {
    return head._1();
  }

  /**
   * Returns the second element of this vector.
   *
   * @return the second element of this vector.
   */
  public A _2() {
    return tail._1();
  }

  /**
   * Returns the third element of this vector.
   *
   * @return the third element of this vector.
   */
  public A _3() {
    return tail._2();
  }

  /**
   * Returns all but the first element of this vector, as a vector-2.
   *
   * @return all but the first element of this vector, as a vector-2.
   */
  public V2<A> tail() {
    return tail;
  }

  /**
   * Returns the first element of this vector, as a product-1.
   *
   * @return the first element of this vector, as a product-1.
   */
  public P1<A> head() {
    return head;
  }

  /**
   * Returns a homogeneous product-3 equivalent to this vector.
   *
   * @return a homogeneous product-3 equivalent to this vector.
   */
  public P3<A, A, A> p() {
    return new P3<A, A, A>() {
      public A _1() {
        return V3.this._1();
      }

      public A _2() {
        return V3.this._2();
      }

      public A _3() {
        return V3.this._3();
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
    return Array.array(_1(), _2(), _3());
  }


  /**
   * Performs function application within a vector (applicative functor pattern).
   *
   * @param vf The vector of functions to apply.
   * @return A new vector after zipping the given vector of functions over this vector.
   */
  public <B> V3<B> apply(final V3<F<A, B>> vf) {
    return new V3<B>(P1.<A, B>apply(head, vf.head()), tail.apply(vf.tail()));
  }

  /**
   * Zips this vector with the given vector using the given function to produce a new vector.
   *
   * @param bs The vector to zip this vector with.
   * @param f  The function to zip this vector and the given vector with.
   * @return A new vector with the results of the function.
   */
  public <B, C> V3<C> zipWith(final F<A, F<B, C>> f, final V3<B> bs) {
    return bs.apply(map(f));
  }

  /**
   * Zips this vector with the given vector to produce a vector of pairs.
   *
   * @param bs The vector to zip this vector with.
   * @return A new vector with a length the same as the shortest of this vector and the given
   *         vector.
   */
  public <B> V3<P2<A, B>> zip(final V3<B> bs) {
    final F<A, F<B, P2<A, B>>> __2 = p2();
    return zipWith(__2, bs);
  }

  /**
   * Zips this vector with the given vector to produce a vector of vectors.
   *
   * @param bs The vector to zip this vector with.
   * @return A new vector of vectors.
   */
  public V3<V2<A>> vzip(final V3<A> bs) {
    final F2<A, A, V2<A>> __2 = V.v2();
    return zipWith(curry(__2), bs);
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
   * Returns a nonempty list with the elements of this vector.
   *
   * @return a nonempty list with the elements of this vector.
   */
  public NonEmptyList<A> toNonEmptyList() {
    return NonEmptyList.nel(head()._1(), tail().toNonEmptyList().toList());
  }

  /**
   * Returns a stream of the elements of this vector.
   *
   * @return a stream of the elements of this vector.
   */
  public Stream<A> toStream() {
    return Stream.cons(head()._1(), new P1<Stream<A>>() {
      public Stream<A> _1() {
        return tail().toStream();
      }
    });
  }

  /**
   * Maps the given function across this vector.
   *
   * @param f The function to map across this vector.
   * @return A new vector after the given function has been applied to each element.
   */
  public <B> V3<B> map(final F<A, B> f) {
    return new V3<B>(head().map(f), tail().map(f));
  }

  /**
   * Returns a function that transforms a vector-3 to a stream of its elements.
   *
   * @return a function that transforms a vector-3 to a stream of its elements.
   */
  public static <A> F<V3<A>, Stream<A>> toStream_() {
    return new F<V3<A>, Stream<A>>() {
      public Stream<A> f(final V3<A> v) {
        return v.toStream();
      }
    };
  }

  /**
   * Returns a function that transforms a vector-3 to the equivalent product-3.
   *
   * @return a function that transforms a vector-3 to the equivalent product-3.
   */
  public static <A> F<V3<A>, P3<A, A, A>> p_() {
    return new F<V3<A>, P3<A, A, A>>() {
      public P3<A, A, A> f(final V3<A> v) {
        return v.p();
      }
    };
  }

}