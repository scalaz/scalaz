package fj.data;

import static fj.Function.constant;
import static fj.Function.identity;
import static fj.Function.curry;
import static fj.P.p;
import static fj.P.p2;
import fj.Effect;
import fj.F;
import fj.F2;
import fj.P;
import fj.P1;
import fj.P2;
import fj.Unit;
import static fj.Unit.unit;
import static fj.data.Option.none;
import static fj.data.Option.some;
import static fj.data.List.iterableList;

import static java.lang.Math.min;
import static java.lang.System.arraycopy;
import java.util.Collection;
import java.util.AbstractCollection;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Provides an interface to arrays.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class Array<A> implements Iterable<A> {
  private final A[] a;

  private Array(final A[] a) {
    this.a = a;
  }

  /**
   * Returns an iterator for this array. This method exists to permit the use in a <code>for</code>-each loop.
   *
   * @return A iterator for this array.
   */
  public Iterator<A> iterator() {
    return toCollection().iterator();
  }

  /**
   * Returns the element at the given index if it exists, fails otherwise.
   *
   * @param index The index at which to get the element to return.
   * @return The element at the given index if it exists, fails otherwise.
   */
  public A get(final int index) {
    return a[index];
  }

  /**
   * Sets the element at the given index to the given value.
   *
   * @param index The index at which to set the given value.
   * @param a     The value to set at the given index.
   * @return The unit value.
   */
  public Unit set(final int index, final A a) {
    this.a[index] = a;
    return unit();
  }

  /**
   * Returns the length of this array.
   *
   * @return The length of this array.
   */
  public int length() {
    return a.length;
  }

  public ImmutableProjection<A> immutable() {
    return new ImmutableProjection<A>(this);
  }

  /**
   * Returns <code>true</code> is this array is empty, <code>false</code> otherwise.
   *
   * @return <code>true</code> is this array is empty, <code>false</code> otherwise.
   */
  public boolean isEmpty() {
    return a.length == 0;
  }

  /**
   * Returns <code>false</code> is this array is empty, <code>true</code> otherwise.
   *
   * @return <code>false</code> is this array is empty, <code>true</code> otherwise.
   */
  public boolean isNotEmpty() {
    return a.length != 0;
  }

  /**
   * Returns the underlying primitive array.
   *
   * @return The underlying primitive array.
   */
  public A[] array() {
    //noinspection ReturnOfCollectionOrArrayField
    return a;
  }

  /**
   * Returns an option projection of this array; <code>None</code> if empty, or the first element in
   * <code>Some</code>.
   *
   * @return An option projection of this array.
   */
  public Option<A> toOption() {
    return a.length == 0 ? Option.<A>none() : some(a[0]);
  }

  /**
   * Returns an either projection of this array; the given argument in <code>Left</code> if empty,
   * or the first element in <code>Right</code>.
   *
   * @param x The value to return in left if this array is empty.
   * @return An either projection of this array.
   */
  public <X> Either<X, A> toEither(final P1<X> x) {
    return a.length == 0 ? Either.<X, A>left(x._1()) : Either.<X, A>right(a[0]);
  }

  /**
   * Returns a list projection of this array.
   *
   * @return A list projection of this array.
   */
  public List<A> toList() {
    List<A> x = List.nil();

    for (int i = a.length - 1; i >= 0; i--) {
      x = x.cons(a[i]);
    }

    return x;
  }

  /**
   * Returns a stream projection of this array.
   *
   * @return A stream projection of this array.
   */
  public Stream<A> toStream() {
    Stream<A> x = Stream.nil();

    for (int i = a.length - 1; i >= 0; i--) {
      x = x.cons(a[i]);
    }

    return x;
  }

  /**
   * Maps the given function across this array.
   *
   * @param f The function to map across this array.
   * @return A new array after the given function has been applied to each element.
   */
  @SuppressWarnings({"unchecked"})
  public <B> Array<B> map(final F<A, B> f) {
    final B[] bs = (B[]) new Object[a.length];

    for (int i = 0; i < a.length; i++) {
      bs[i] = f.f(a[i]);
    }

    return new Array<B>(bs);
  }

  /**
   * Filters elements from this array by returning only elements which produce <code>true</code>
   * when the given function is applied to them.
   *
   * @param f The predicate function to filter on.
   * @return A new array whose elements all match the given predicate.
   */
  public Array<A> filter(final F<A, Boolean> f) {
    List<A> x = List.nil();

    for (int i = a.length - 1; i >= 0; i--) {
      if (f.f(a[i]))
        x = x.cons(a[i]);
    }

    return x.toArray();
  }

  /**
   * Performs a side-effect for each element of this array.
   *
   * @param f The side-effect to perform for the given element.
   * @return The unit value.
   */
  public Unit foreach(final F<A, Unit> f) {
    for (final A x : a) {
      f.f(x);
    }

    return unit();
  }

  /**
   * Performs a side-effect for each element of this array.
   *
   * @param f The side-effect to perform for the given element.
   */
  public void foreach(final Effect<A> f) {
    for (final A x : a) {
      f.e(x);
    }
  }

  /**
   * Performs a right-fold reduction across this array. This function runs in constant stack space.
   *
   * @param f The function to apply on each element of the array.
   * @param b The beginning value to start the application from.
   * @return The final result after the right-fold reduction.
   */
  public <B> B foldRight(final F<A, F<B, B>> f, final B b) {
    B x = b;

    for (int i = a.length - 1; i >= 0; i--)
      x = f.f(a[i]).f(x);

    return x;
  }

  /**
   * Performs a right-fold reduction across this array. This function runs in constant stack space.
   *
   * @param f The function to apply on each element of the array.
   * @param b The beginning value to start the application from.
   * @return The final result after the right-fold reduction.
   */
  public <B> B foldRight(final F2<A, B, B> f, final B b) {
    return foldRight(curry(f), b);
  }

  /**
   * Performs a left-fold reduction across this array. This function runs in constant space.
   *
   * @param f The function to apply on each element of the array.
   * @param b The beginning value to start the application from.
   * @return The final result after the left-fold reduction.
   */
  public <B> B foldLeft(final F<B, F<A, B>> f, final B b) {
    B x = b;

    for (final A aa : a)
      x = f.f(x).f(aa);

    return x;
  }

  /**
   * Performs a left-fold reduction across this array. This function runs in constant space.
   *
   * @param f The function to apply on each element of the array.
   * @param b The beginning value to start the application from.
   * @return The final result after the left-fold reduction.
   */
  public <B> B foldLeft(final F2<B, A, B> f, final B b) {
    return foldLeft(curry(f), b);
  }

  /**
   * Binds the given function across each element of this array with a final join.
   *
   * @param f The function to apply to each element of this array.
   * @return A new array after performing the map, then final join.
   */
  @SuppressWarnings({"unchecked"})
  public <B> Array<B> bind(final F<A, Array<B>> f) {
    List<Array<B>> x = List.nil();
    int len = 0;

    for (int i = a.length - 1; i >= 0; i--) {
      final Array<B> bs = f.f(a[i]);
      len = len + bs.length();
      x = x.cons(bs);
    }

    final B[] bs = (B[]) new Object[len];

    x.foreach(new F<Array<B>, Unit>() {
      private int i;

      public Unit f(final Array<B> x) {
        arraycopy(x.a, 0, bs, i, x.a.length);
        i = i + x.a.length;
        return unit();
      }
    });

    return new Array<B>(bs);
  }

  /**
   * Performs a bind across each array element, but ignores the element value each time.
   *
   * @param bs The array to apply in the final join.
   * @return A new array after the final join.
   */
  public <B> Array<B> sequence(final Array<B> bs) {
    final F<A, Array<B>> c = constant(bs);
    return bind(c);
  }

  /**
   * Performs function application within an array (applicative functor pattern).
   *
   * @param lf The array of functions to apply.
   * @return A new array after applying the given array of functions through this array.
   */
  public <B> Array<B> apply(final Array<F<A, B>> lf) {
    return lf.bind(new F<F<A, B>, Array<B>>() {
      public Array<B> f(final F<A, B> f) {
        return map(new F<A, B>() {
          public B f(final A a) {
            return f.f(a);
          }
        });
      }
    });
  }

  /**
   * Reverse this array in constant stack space.
   *
   * @return A new array that is the reverse of this one.
   */
  @SuppressWarnings({"unchecked"})
  public Array<A> reverse() {
    final A[] x = (A[]) new Object[a.length];

    for (int i = 0; i < a.length; i++) {
      x[a.length - 1 - i] = a[i];
    }

    return new Array<A>(x);
  }

  /**
   * Appends the given array to this array.
   *
   * @param aas The array to append to this one.
   * @return A new array that has appended the given array.
   */
  @SuppressWarnings({"unchecked"})
  public Array<A> append(final Array<A> aas) {
    final A[] x = (A[]) new Object[a.length + aas.a.length];

    arraycopy(a, 0, x, 0, a.length);
    arraycopy(aas.a, 0, x, a.length, aas.a.length);

    return new Array<A>(x);
  }

  /**
   * Returns an empty array.
   *
   * @return An empty array.
   */
  @SuppressWarnings({"unchecked"})
  public static <A> Array<A> empty() {
    return new Array<A>((A[]) new Object[0]);
  }

  /**
   * Constructs an array from the given elements.
   *
   * @param a The elements to construct the array with.
   * @return A new array of the given elements.
   */
  public static <A> Array<A> array(final A... a) {
    return new Array<A>(a);
  }

  /**
   * Joins the given array of arrays using a bind operation.
   *
   * @param o The array of arrays to join.
   * @return A new array that is the join of the given arrays.
   */
  public static <A> Array<A> join(final Array<Array<A>> o) {
    final F<Array<A>, Array<A>> id = identity();
    return o.bind(id);
  }

  /**
   * A first-class version of join
   *
   * @return A function that joins a array of arrays using a bind operation.
   */
  public static <A> F<Array<Array<A>>, Array<A>> join() {
    return new F<Array<Array<A>>, Array<A>>() {
      public Array<A> f(final Array<Array<A>> as) {
        return join(as);
      }
    };
  }

  /**
   * Returns <code>true</code> if the predicate holds for all of the elements of this array,
   * <code>false</code> otherwise (<code>true</code> for the empty array).
   *
   * @param f the predicate function to test on each element of this array.
   * @return <code>true</code> if the predicate holds for all of the elements of this array,
   *         <code>false</code> otherwise.
   */
  public boolean forall(final F<A, Boolean> f) {
    for (final A x : a)
      if (!f.f(x))
        return false;

    return true;
  }

  /**
   * Returns <code>true</code> if the predicate holds for at least one of the elements of this
   * array, <code>false</code> otherwise (<code>false</code> for the empty array).
   *
   * @param f the predicate function to test on the elements of this array.
   * @return <code>true</code> if the predicate holds for at least one of the elements of this
   *         array.
   */
  public boolean exists(final F<A, Boolean> f) {
    for (final A x : a)
      if (f.f(x))
        return true;

    return false;
  }

  /**
   * Finds the first occurrence of an element that matches the given predicate or no value if no
   * elements match.
   *
   * @param f The predicate function to test on elements of this array.
   * @return The first occurrence of an element that matches the given predicate or no value if no
   *         elements match.
   */
  public Option<A> find(final F<A, Boolean> f) {
    for (final A x : a)
      if (f.f(x))
        return some(x);

    return none();
  }

  /**
   * Returns an array of integers from the given <code>from</code> value (inclusive) to the given
   * <code>to</code> value (exclusive).
   *
   * @param from The minimum value for the array (inclusive).
   * @param to   The maximum value for the array (exclusive).
   * @return An array of integers from the given <code>from</code> value (inclusive) to the given
   *         <code>to</code> value (exclusive).
   */
  public static Array<Integer> range(final int from, final int to) {
    if (from >= to)
      return Array.empty();
    else {
      final Array<Integer> a = new Array<Integer>(new Integer[to - from]);

      for (int i = from; i < to; i++)
        a.set(i - from, i);

      return a;
    }
  }

  /**
   * Zips this array with the given array using the given function to produce a new array. If this
   * array and the given array have different lengths, then the longer array is normalised so this
   * function never fails.
   *
   * @param bs The array to zip this array with.
   * @param f  The function to zip this array and the given array with.
   * @return A new array with a length the same as the shortest of this array and the given array.
   */
  @SuppressWarnings({"unchecked"})
  public <B, C> Array<C> zipWith(final Array<B> bs, final F<A, F<B, C>> f) {
    final int len = min(a.length, bs.length());
    final Array<C> x = new Array<C>((C[]) new Object[len]);

    for (int i = 0; i < len; i++) {
      x.set(i, f.f(get(i)).f(bs.get(i)));
    }

    return x;
  }

  /**
   * Zips this array with the given array to produce an array of pairs. If this array and the given
   * array have different lengths, then the longer array is normalised so this function never fails.
   *
   * @param bs The array to zip this array with.
   * @return A new array with a length the same as the shortest of this array and the given array.
   */
  public <B> Array<P2<A, B>> zip(final Array<B> bs) {
    final F<A, F<B, P2<A, B>>> __2 = p2();
    return zipWith(bs, __2);
  }

  /**
   * Zips this array with the index of its element as a pair.
   *
   * @return A new array with the same length as this array.
   */
  public Array<P2<A, Integer>> zipIndex() {
    return zipWith(range(0, length()), new F<A, F<Integer, P2<A, Integer>>>() {
      public F<Integer, P2<A, Integer>> f(final A a) {
        return new F<Integer, P2<A, Integer>>() {
          public P2<A, Integer> f(final Integer i) {
            return p(a, i);
          }
        };
      }
    });
  }

  /**
   * Projects an immutable collection of this array.
   *
   * @return An immutable collection of this array.
   */
  public Collection<A> toCollection() {
    return new AbstractCollection<A>() {
      public Iterator<A> iterator() {
        return new Iterator<A>() {
          private int i;

          public boolean hasNext() {
            return i < a.length;
          }

          public A next() {
            if (i >= a.length)
              throw new NoSuchElementException();
            else {
              final A aa = a[i];
              i++;
              return aa;
            }
          }

          public void remove() {
            throw new UnsupportedOperationException();
          }
        };
      }

      public int size() {
        return a.length;
      }
    };
  }

  /**
   * Takes the given iterable to an array.
   *
   * @param i The iterable to take to an array.
   * @return An array from the given iterable.
   */
  public static <A> Array<A> iterableArray(final Iterable<A> i) {
    return iterableList(i).toArray();
  }

  /**
   * Transforms an array of pairs into an array of first components and an array of second components.
   * @param xs The array of pairs to transform.
   * @return An array of first components and an array of second components.
   */
  @SuppressWarnings({"unchecked"})
  public static <A, B> P2<Array<A>, Array<B>> unzip(final Array<P2<A, B>> xs) {
    final int len = xs.length();
    final Array<A> aa = new Array<A>((A[])new Object[len]);
    final Array<B> ab = new Array<B>((B[])new Object[len]);
    for (int i = len - 1; i >= 0; i--) {
      final P2<A, B> p = xs.get(i);
      aa.set(i, p._1());
      ab.set(i, p._2());
    }
    return P.p(aa, ab);
  }

  /**
   * Projects an array by providing only operations which do not mutate.
   */
  public final class ImmutableProjection<A> implements Iterable<A> {
    private final Array<A> a;

    private ImmutableProjection(final Array<A> a) {
      this.a = a;
    }

    /**
     * Returns an iterator for this array. This method exists to permit the use in a <code>for</code>-each loop.
     *
     * @return A iterator for this array.
     */
    public Iterator<A> iterator() {
      return a.iterator();
    }

    /**
     * Returns the element at the given index if it exists, fails otherwise.
     *
     * @param index The index at which to get the element to return.
     * @return The element at the given index if it exists, fails otherwise.
     */
    public A get(final int index) {
      return a.get(index);
    }

    /**
     * Returns the length of this array.
     *
     * @return The length of this array.
     */
    public int length() {
      return a.length();
    }

    /**
     * Returns <code>true</code> is this array is empty, <code>false</code> otherwise.
     *
     * @return <code>true</code> is this array is empty, <code>false</code> otherwise.
     */
    public boolean isEmpty() {
      return a.isEmpty();
    }

    /**
     * Returns <code>false</code> is this array is empty, <code>true</code> otherwise.
     *
     * @return <code>false</code> is this array is empty, <code>true</code> otherwise.
     */
    public boolean isNotEmpty() {
      return a.isNotEmpty();
    }

    /**
     * Returns an option projection of this array; <code>None</code> if empty, or the first element
     * in <code>Some</code>.
     *
     * @return An option projection of this array.
     */
    public Option<A> toOption() {
      return a.toOption();
    }

    /**
     * Returns an either projection of this array; the given argument in <code>Left</code> if empty,
     * or the first element in <code>Right</code>.
     *
     * @param x The value to return in left if this array is empty.
     * @return An either projection of this array.
     */
    public <X> Either<X, A> toEither(final P1<X> x) {
      return a.toEither(x);
    }

    /**
     * Returns a list projection of this array.
     *
     * @return A list projection of this array.
     */
    public List<A> toList() {
      return a.toList();
    }

    /**
     * Returns a stream projection of this array.
     *
     * @return A stream projection of this array.
     */
    public Stream<A> toStream() {
      return a.toStream();
    }

    /**
     * Maps the given function across this array.
     *
     * @param f The function to map across this array.
     * @return A new array after the given function has been applied to each element.
     */
    @SuppressWarnings({"unchecked"})
    public <B> Array<B> map(final F<A, B> f) {
      return a.map(f);
    }

    /**
     * Filters elements from this array by returning only elements which produce <code>true</code>
     * when the given function is applied to them.
     *
     * @param f The predicate function to filter on.
     * @return A new array whose elements all match the given predicate.
     */
    public Array<A> filter(final F<A, Boolean> f) {
      return a.filter(f);
    }

    /**
     * Performs a side-effect for each element of this array.
     *
     * @param f The side-effect to perform for the given element.
     * @return The unit value.
     */
    public Unit foreach(final F<A, Unit> f) {
      return a.foreach(f);
    }

    /**
     * Performs a right-fold reduction across this array. This function uses O(length) stack space.
     *
     * @param f The function to apply on each element of the array.
     * @param b The beginning value to start the application from.
     * @return The final result after the right-fold reduction.
     */
    public <B> B foldRight(final F<A, F<B, B>> f, final B b) {
      return a.foldRight(f, b);
    }

    /**
     * Performs a left-fold reduction across this array. This function runs in constant space.
     *
     * @param f The function to apply on each element of the array.
     * @param b The beginning value to start the application from.
     * @return The final result after the left-fold reduction.
     */
    public <B> B foldLeft(final F<B, F<A, B>> f, final B b) {
      return a.foldLeft(f, b);
    }

    /**
     * Binds the given function across each element of this array with a final join.
     *
     * @param f The function to apply to each element of this array.
     * @return A new array after performing the map, then final join.
     */
    @SuppressWarnings({"unchecked"})
    public <B> Array<B> bind(final F<A, Array<B>> f) {
      return a.bind(f);
    }

    /**
     * Performs a bind across each array element, but ignores the element value each time.
     *
     * @param bs The array to apply in the final join.
     * @return A new array after the final join.
     */
    public <B> Array<B> sequence(final Array<B> bs) {
      return a.sequence(bs);
    }

    /**
     * Performs function application within an array (applicative functor pattern).
     *
     * @param lf The array of functions to apply.
     * @return A new array after applying the given array of functions through this array.
     */
    public <B> Array<B> apply(final Array<F<A, B>> lf) {
      return a.apply(lf);
    }

    /**
     * Reverse this array in constant stack space.
     *
     * @return A new array that is the reverse of this one.
     */
    @SuppressWarnings({"unchecked"})
    public Array<A> reverse() {
      return a.reverse();
    }

    /**
     * Appends the given array to this array.
     *
     * @param aas The array to append to this one.
     * @return A new array that has appended the given array.
     */
    @SuppressWarnings({"unchecked"})
    public Array<A> append(final Array<A> aas) {
      return a.append(aas);
    }

    /**
     * Projects an immutable collection of this array.
     *
     * @return An immutable collection of this array.
     */
    public Collection<A> toCollection() {
      return a.toCollection();
    }
  }
}
