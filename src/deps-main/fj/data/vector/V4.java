package fj.data.vector;

import fj.F;
import fj.F2;
import fj.P1;
import fj.P2;
import fj.P3;
import fj.P4;
import static fj.Function.curry;
import static fj.P.p2;
import fj.data.Array;
import fj.data.NonEmptyList;
import fj.data.Stream;

import java.util.Iterator;

/**
 * A vector-4.
 */
public final class V4<A> implements Iterable<A> {

  private final V3<A> tail;
  private final P1<A> head;

  private V4(final P1<A> head, final V3<A> tail) {
    this.head = head;
    this.tail = tail;
  }

  /**
   * Creates a vector-4 from a homogeneous product-4.
   *
   * @param p The product-4 from which to create a vector.
   * @return A new vector-4.
   */
  public static <A> V4<A> p(final P4<A, A, A, A> p) {
    return new V4<A>(new P1<A>() {
      public A _1() {
        return p._1();
      }
    }, V3.p(new P3<A, A, A>() {
      public A _1() {
        return p._2();
      }

      public A _2() {
        return p._3();
      }

      public A _3() {
        return p._4();
      }
    }));
  }

  /**
   * Creates a vector-4 from a head and a tail.
   *
   * @param head The value to put as the first element of the vector.
   * @param tail The vector representing all but the first element of the new vector.
   * @return The new vector.
   */
  public static <A> V4<A> cons(final P1<A> head, final V3<A> tail) {
    return new V4<A>(head, tail);
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
   * Returns the fourth element of this vector.
   *
   * @return the fourth element of this vector.
   */
  public A _4() {
    return tail._3();
  }

  /**
   * Returns all but the first element of this vector, as a vector-3.
   *
   * @return all but the first element of this vector, as a vector-3.
   */
  public V3<A> tail() {
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
   * Returns an iterator for the elements of this vector.
   *
   * @return an iterator for the elements of this vector.
   */
  public Iterator<A> iterator() {
    return toStream().iterator();
  }

  /**
   * Returns a homogeneous product-4 equivalent to this vector.
   *
   * @return a homogeneous product-4 equivalent to this vector.
   */
  public P4<A, A, A, A> p() {
    return new P4<A, A, A, A>() {
      public A _1() {
        return V4.this._1();
      }

      public A _2() {
        return V4.this._2();
      }

      public A _3() {
        return V4.this._3();
      }

      public A _4() {
        return V4.this._4();
      }
    };
  }

  /**
   * Returns a nonempty list with the elements of this vector.
   *
   * @return a nonempty list with the elements of this vector.
   */
  public NonEmptyList<A> toNonEmptyList() {
    return NonEmptyList.nel(_1(), tail.toNonEmptyList().toList());
  }

  /**
   * Returns a stream of the elements of this vector.
   *
   * @return a stream of the elements of this vector.
   */
  public Stream<A> toStream() {
    return Stream.cons(head._1(), new P1<Stream<A>>() {
      public Stream<A> _1() {
        return tail.toStream();
      }
    });
  }

  /**
   * Returns an array with the elements of this vector.
   *
   * @return an array with the elements of this vector.
   */
  @SuppressWarnings("unchecked")
  public Array<A> toArray() {
    return Array.array(_1(), _2(), _3(), _4());
  }

  /**
   * Maps the given function across this vector.
   *
   * @param f The function to map across this vector.
   * @return A new vector after the given function has been applied to each element.
   */
  public <B> V4<B> map(final F<A, B> f) {
    return new V4<B>(head.map(f), tail.map(f));
  }

  /**
   * Performs function application within a vector (applicative functor pattern).
   *
   * @param vf The vector of functions to apply.
   * @return A new vector after zipping the given vector of functions over this vector.
   */
  public <B> V4<B> apply(final V4<F<A, B>> vf) {
    return new V4<B>(P1.<A, B>apply(head, vf.head()), tail.apply(vf.tail()));
  }

  /**
   * Zips this vector with the given vector using the given function to produce a new vector.
   *
   * @param bs The vector to zip this vector with.
   * @param f  The function to zip this vector and the given vector with.
   * @return A new vector with the results of the function.
   */
  public <B, C> V4<C> zipWith(final F<A, F<B, C>> f, final V4<B> bs) {
    return bs.apply(map(f));
  }

  /**
   * Zips this vector with the given vector to produce a vector of pairs.
   *
   * @param bs The vector to zip this vector with.
   * @return A new vector with a length the same as the shortest of this vector and the given
   *         vector.
   */
  public <B> V4<P2<A, B>> zip(final V4<B> bs) {
    final F<A, F<B, P2<A, B>>> __2 = p2();
    return zipWith(__2, bs);
  }

  /**
   * Zips this vector with the given vector to produce a vector of vectors.
   *
   * @param bs The vector to zip this vector with.
   * @return A new vector of vectors.
   */
  public V4<V2<A>> vzip(final V4<A> bs) {
    final F2<A, A, V2<A>> __2 = V.v2();
    return zipWith(curry(__2), bs);
  }

  /**
   * Returns a function that transforms a vector-4 to a stream of its elements.
   *
   * @return a function that transforms a vector-4 to a stream of its elements.
   */
  public static <A> F<V4<A>, Stream<A>> toStream_() {
    return new F<V4<A>, Stream<A>>() {
      public Stream<A> f(final V4<A> v) {
        return v.toStream();
      }
    };
  }

  /**
   * Returns a function that transforms a vector-4 to the equivalent product-4.
   *
   * @return a function that transforms a vector-4 to the equivalent product-4.
   */
  public static <A> F<V4<A>, P4<A, A, A, A>> p_() {
    return new F<V4<A>, P4<A, A, A, A>>() {
      public P4<A, A, A, A> f(final V4<A> v) {
        return v.p();
      }
    };
  }


}