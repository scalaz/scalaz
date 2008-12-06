package fj.data.vector;

import fj.F;
import fj.F2;
import fj.P1;
import fj.P2;
import fj.P5;
import fj.P6;
import static fj.Function.curry;
import static fj.P.p2;
import fj.data.Array;
import fj.data.NonEmptyList;
import fj.data.Stream;

import java.util.Iterator;

/**
 * A vector-6.
 */
public final class V6<A> implements Iterable<A> {

  private final V5<A> tail;
  private final P1<A> head;

  private V6(final P1<A> head, final V5<A> tail) {
    this.head = head;
    this.tail = tail;
  }

  /**
   * Creates a vector-6 from a homogeneous product-6.
   *
   * @param p The product-6 from which to create a vector.
   * @return A new vector-6.
   */
  public static <A> V6<A> p(final P6<A, A, A, A, A, A> p) {
    return new V6<A>(new P1<A>() {
      public A _1() {
        return p._1();
      }
    }, V5.p(new P5<A, A, A, A, A>() {
      public A _1() {
        return p._2();
      }

      public A _2() {
        return p._3();
      }

      public A _3() {
        return p._4();
      }

      public A _4() {
        return p._5();
      }

      public A _5() {
        return p._6();
      }
    }));
  }

  /**
   * Creates a vector-6 from a head and a tail.
   *
   * @param head The value to put as the first element of the vector.
   * @param tail The vector representing all but the first element of the new vector.
   * @return The new vector.
   */
  public static <A> V6<A> cons(final P1<A> head, final V5<A> tail) {
    return new V6<A>(head, tail);
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
   * Returns the fifth element of this vector.
   *
   * @return the fifth element of this vector.
   */
  public A _5() {
    return tail._4();
  }

  /**
   * Returns the sixth element of this vector.
   *
   * @return the sixth element of this vector.
   */
  public A _6() {
    return tail._5();
  }

  /**
   * Returns all but the first element of this vector, as a vector-5.
   *
   * @return all but the first element of this vector, as a vector-5.
   */
  public V5<A> tail() {
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
   * Returns a homogeneous product-6 equivalent to this vector.
   *
   * @return a homogeneous product-6 equivalent to this vector.
   */
  public P6<A, A, A, A, A, A> p() {
    return new P6<A, A, A, A, A, A>() {
      public A _1() {
        return V6.this._1();
      }

      public A _2() {
        return V6.this._2();
      }

      public A _3() {
        return V6.this._3();
      }

      public A _4() {
        return V6.this._4();
      }

      public A _5() {
        return V6.this._5();
      }

      public A _6() {
        return V6.this._6();
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
    return Array.array(_1(), _2(), _3(), _4(), _5(), _6());
  }

  /**
   * Maps the given function across this vector.
   *
   * @param f The function to map across this vector.
   * @return A new vector after the given function has been applied to each element.
   */
  public <B> V6<B> map(final F<A, B> f) {
    return new V6<B>(head.map(f), tail.map(f));
  }

  /**
   * Performs function application within a vector (applicative functor pattern).
   *
   * @param vf The vector of functions to apply.
   * @return A new vector after zipping the given vector of functions over this vector.
   */
  public <B> V6<B> apply(final V6<F<A, B>> vf) {
    return new V6<B>(P1.<A, B>apply(head, vf.head()), tail.apply(vf.tail()));
  }

  /**
   * Zips this vector with the given vector using the given function to produce a new vector.
   *
   * @param bs The vector to zip this vector with.
   * @param f  The function to zip this vector and the given vector with.
   * @return A new vector with the results of the function.
   */
  public <B, C> V6<C> zipWith(final F<A, F<B, C>> f, final V6<B> bs) {
    return bs.apply(map(f));
  }

  /**
   * Zips this vector with the given vector to produce a vector of pairs.
   *
   * @param bs The vector to zip this vector with.
   * @return A new vector with a length the same as the shortest of this vector and the given
   *         vector.
   */
  public <B> V6<P2<A, B>> zip(final V6<B> bs) {
    final F<A, F<B, P2<A, B>>> __2 = p2();
    return zipWith(__2, bs);
  }

  /**
   * Zips this vector with the given vector to produce a vector of vectors.
   *
   * @param bs The vector to zip this vector with.
   * @return A new vector of vectors.
   */
  public V6<V2<A>> vzip(final V6<A> bs) {
    final F2<A, A, V2<A>> __2 = V.v2();
    return zipWith(curry(__2), bs);
  }

  /**
   * Returns a function that transforms a vector-6 to a stream of its elements.
   *
   * @return a function that transforms a vector-6 to a stream of its elements.
   */
  public static <A> F<V6<A>, Stream<A>> toStream_() {
    return new F<V6<A>, Stream<A>>() {
      public Stream<A> f(final V6<A> v) {
        return v.toStream();
      }
    };
  }

  /**
   * Returns a function that transforms a vector-6 to the equivalent product-6.
   *
   * @return a function that transforms a vector-6 to the equivalent product-6.
   */
  public static <A> F<V6<A>, P6<A, A, A, A, A, A>> p_() {
    return new F<V6<A>, P6<A, A, A, A, A, A>>() {
      public P6<A, A, A, A, A, A> f(final V6<A> v) {
        return v.p();
      }
    };
  }

}