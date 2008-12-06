package fj.data;

import static fj.Bottom.error;
import fj.Effect;
import fj.F;
import fj.F2;
import fj.F3;
import fj.P;
import fj.P1;
import fj.P2;
import fj.Unit;
import static fj.Function.curry;
import static fj.Function.constant;
import static fj.Function.identity;
import static fj.Function.compose;
import static fj.P.p;
import static fj.P.p2;
import static fj.Unit.unit;
import static fj.data.Array.array;
import static fj.data.Option.none;
import static fj.data.Option.some;
import static fj.function.Booleans.not;
import fj.pre.Ord;
import fj.pre.Equal;
import fj.pre.Monoid;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.AbstractCollection;

/**
 * Provides an in-memory, immutable, singly linked list.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public abstract class List<A> implements Iterable<A> {
  private List() {

  }

  /**
   * Returns an iterator for this list. This method exists to permit the use in a <code>for</code>-each loop.
   *
   * @return A iterator for this list.
   */
  public Iterator<A> iterator() {
    return toCollection().iterator();
  }

  /**
   * The first element of the linked list or fails for the empty list.
   *
   * @return The first element of the linked list or fails for the empty list.
   */
  public abstract A head();

  /**
   * The list without the first element or fails for the empty list.
   *
   * @return The list without the first element or fails for the empty list.
   */
  public abstract List<A> tail();

  /**
   * The length of this list.
   *
   * @return The length of this list.
   */
  public int length() {
    return foldLeft(new F<Integer, F<A, Integer>>() {
      public F<A, Integer> f(final Integer i) {
        return new F<A, Integer>() {
          public Integer f(final A a) {
            return i + 1;
          }
        };
      }
    }, 0);
  }

  /**
   * Returns <code>true</code> if this list is empty, <code>false</code> otherwise.
   *
   * @return <code>true</code> if this list is empty, <code>false</code> otherwise.
   */
  public boolean isEmpty() {
    return this instanceof Nil;
  }

  /**
   * Returns <code>false</code> if this list is empty, <code>true</code> otherwise.
   *
   * @return <code>false</code> if this list is empty, <code>true</code> otherwise.
   */
  public boolean isNotEmpty() {
    return this instanceof Cons;
  }

  /**
   * Performs a reduction on this list using the given arguments.
   *
   * @param nil  The value to return if this list is empty.
   * @param cons The function to apply to the head and tail of this list if it is not empty.
   * @return A reduction on this list.
   */
  public <B> B list(final B nil, final F<A, F<List<A>, B>> cons) {
    return isEmpty() ? nil : cons.f(head()).f(tail());
  }

  /**
   * Returns the head of this list if there is one or the given argument if this list is empty.
   *
   * @param a The argument to return if this list is empty.
   * @return The head of this list if there is one or the given argument if this list is empty.
   */
  public A orHead(final P1<A> a) {
    return isEmpty() ? a._1() : head();
  }

  /**
   * Returns the tail of this list if there is one or the given argument if this list is empty.
   *
   * @param as The argument to return if this list is empty.
   * @return The tail of this list if there is one or the given argument if this list is empty.
   */
  public List<A> orTail(final P1<List<A>> as) {
    return isEmpty() ? as._1() : tail();
  }

  /**
   * Returns an option projection of this list; <code>None</code> if empty, or the first element in
   * <code>Some</code>.
   *
   * @return An option projection of this list.
   */
  public Option<A> toOption() {
    return isEmpty() ? Option.<A>none() : some(head());
  }

  /**
   * Returns an either projection of this list; the given argument in <code>Left</code> if empty, or
   * the first element in <code>Right</code>.
   *
   * @param x The value to return in left if this list is empty.
   * @return An either projection of this list.
   */
  public <X> Either<X, A> toEither(final P1<X> x) {
    return isEmpty() ? Either.<X, A>left(x._1()) : Either.<X, A>right(head());
  }

  /**
   * Returns a stream projection of this list.
   *
   * @return A stream projection of this list.
   */
  public Stream<A> toStream() {
    final Stream<A> nil = Stream.nil();
    return foldRight(new F<A, F<Stream<A>, Stream<A>>>() {
      public F<Stream<A>, Stream<A>> f(final A a) {
        return new F<Stream<A>, Stream<A>>() {
          public Stream<A> f(final Stream<A> as) {
            return as.cons(a);
          }
        };
      }
    }, nil);
  }

  /**
   * Returns a array projection of this list.
   *
   * @return A array projection of this list.
   */
  @SuppressWarnings({"unchecked"})
  public Array<A> toArray() {
    final A[] a = (A[]) new Object[length()];
    List<A> x = this;
    for (int i = 0; i < length(); i++) {
      a[i] = x.head();
      x = x.tail();
    }

    return array(a);
  }

  /**
   * Returns a array projection of this list.
   *
   * @param c The class type of the array to return.
   * @return A array projection of this list.
   */
  @SuppressWarnings({"unchecked"})
  public Array<A> toArray(final Class<A[]> c) {
    final A[] a = (A[]) java.lang.reflect.Array.newInstance(c.getComponentType(), length());
    List<A> x = this;
    for (int i = 0; i < length(); i++) {
      a[i] = x.head();
      x = x.tail();
    }

    return array(a);
  }

  /**
   * Prepends (cons) the given element to this list to product a new list.
   *
   * @param a The element to prepend.
   * @return A new list with the given element at the head.
   */
  public List<A> cons(final A a) {
    return new Cons<A>(a, this);
  }

  /**
   * Maps the given function across this list.
   *
   * @param f The function to map across this list.
   * @return A new list after the given function has been applied to each element.
   */
  public <B> List<B> map(final F<A, B> f) {
    final Buffer<B> bs = Buffer.empty();

    for (List<A> xs = this; xs.isNotEmpty(); xs = xs.tail()) {
      bs.snoc(f.f(xs.head()));
    }

    return bs.toList();
  }

  /**
   * Performs a side-effect for each element of this list.
   *
   * @param f The side-effect to perform for the given element.
   * @return The unit value.
   */
  public Unit foreach(final F<A, Unit> f) {
    for (List<A> xs = this; xs.isNotEmpty(); xs = xs.tail()) {
      f.f(xs.head());
    }

    return unit();
  }

  /**
   * Performs a side-effect for each element of this list.
   *
   * @param f The side-effect to perform for the given element.
   */
  public void foreach(final Effect<A> f) {
    for (List<A> xs = this; xs.isNotEmpty(); xs = xs.tail()) {
      f.e(xs.head());
    }
  }

  /**
   * Filters elements from this list by returning only elements which produce <code>true</code> when
   * the given function is applied to them.
   *
   * @param f The predicate function to filter on.
   * @return A new list whose elements all match the given predicate.
   */
  public List<A> filter(final F<A, Boolean> f) {
    final Buffer<A> b = Buffer.empty();

    for (List<A> xs = this; xs.isNotEmpty(); xs = xs.tail()) {
      final A h = xs.head();
      if (f.f(h)) {
        b.snoc(h);
      }
    }

    return b.toList();
  }

  /**
   * Filters elements from this list by returning only elements which produce <code>false</code> when
   * the given function is applied to them.
   *
   * @param f The predicate function to filter on.
   * @return A new list whose elements do not match the given predicate.
   */
  public List<A> removeAll(final F<A, Boolean> f) {
    return filter(compose(not, f));
  }

  /**
   * Removes the first element that equals the given object.
   * To remove all matches, use <code>removeAll(e.eq(a))</code>
   *
   * @param a The element to remove
   * @param e An <code>Equals</code> instance for the element's type.
   * @return A new list whose elements do not match the given predicate.
   */
  public List<A> delete(final A a, final Equal<A> e) {
    final P2<List<A>, List<A>> p = span(compose(not, e.eq(a)));
    return p._2().isEmpty() ? p._1() : p._1().append(p._2().tail());
  }

  /**
   * Returns the first elements of the head of this list that match the given predicate function.
   *
   * @param f The predicate function to apply on this list until it finds an element that does not
   *          hold, or the list is exhausted.
   * @return The first elements of the head of this list that match the given predicate function.
   */
  public List<A> takeWhile(final F<A, Boolean> f) {
    final Buffer<A> b = Buffer.empty();
    boolean taking = true;

    for (List<A> xs = this; xs.isNotEmpty() && taking; xs = xs.tail()) {
      final A h = xs.head();
      if (f.f(h)) {
        b.snoc(h);
      } else {
        taking = false;
      }
    }

    return b.toList();
  }

  /**
   * Removes elements from the head of this list that do not match the given predicate function
   * until an element is found that does match or the list is exhausted.
   *
   * @param f The predicate function to apply through this list.
   * @return The list whose first element does not match the given predicate function.
   */
  public List<A> dropWhile(final F<A, Boolean> f) {
    List<A> xs;

    //noinspection StatementWithEmptyBody
    for (xs = this; xs.isNotEmpty() && f.f(xs.head()); xs = xs.tail()) ;

    return xs;
  }

  /**
   * Returns a tuple where the first element is the longest prefix of this list that satisfies
   * the given predicate and the second element is the remainder of the list.
   *
   * @param p A predicate to be satisfied by a prefix of this list.
   * @return A tuple where the first element is the longest prefix of this list that satisfies
   *         the given predicate and the second element is the remainder of the list.
   */
  public P2<List<A>, List<A>> span(final F<A, Boolean> p) {
    final Buffer<A> b = Buffer.empty();
    for (List<A> xs = this; xs.isNotEmpty(); xs = xs.tail()) {
      if (p.f(xs.head()))
        b.snoc(xs.head());
      else
        return P.p(b.toList(), xs);
    }
    return P.p(b.toList(), List.<A>nil());
  }


  /**
   * Binds the given function across each element of this list with a final join.
   *
   * @param f The function to apply to each element of this list.
   * @return A new list after performing the map, then final join.
   */
  public <B> List<B> bind(final F<A, List<B>> f) {
    final Buffer<B> b = Buffer.empty();

    for (List<A> xs = this; xs.isNotEmpty(); xs = xs.tail()) {
      b.append(f.f(xs.head()));
    }

    return b.toList();
  }

  /**
   * Binds the given function across each element of this list and the given list with a final
   * join.
   *
   * @param lb A given list to bind the given function with.
   * @param f  The function to apply to each element of this list and the given list.
   * @return A new list after performing the map, then final join.
   */
  public <B, C> List<C> bind(final List<B> lb, final F<A, F<B, C>> f) {
    return lb.apply(map(f));
  }

  /**
   * Promotes the given function of arity-2 to a function on lists.
   *
   * @param f The functio to promote to a function on lists.
   * @return The given function, promoted to operate on lists.
   */
  public static <A, B, C> F<List<A>, F<List<B>, List<C>>> liftM2(final F<A, F<B, C>> f) {
    return curry(new F2<List<A>, List<B>, List<C>>() {
      public List<C> f(final List<A> as, final List<B> bs) {
        return as.bind(bs, f);
      }
    });
  }

  /**
   * Binds the given function across each element of this list and the given lists with a final
   * join.
   *
   * @param lb A given list to bind the given function with.
   * @param lc A given list to bind the given function with.
   * @param f  The function to apply to each element of this list and the given lists.
   * @return A new list after performing the map, then final join.
   */
  public <B, C, D> List<D> bind(final List<B> lb, final List<C> lc, final F<A, F<B, F<C, D>>> f) {
    return lc.apply(bind(lb, f));
  }

  /**
   * Binds the given function across each element of this list and the given lists with a final
   * join.
   *
   * @param lb A given list to bind the given function with.
   * @param lc A given list to bind the given function with.
   * @param ld A given list to bind the given function with.
   * @param f  The function to apply to each element of this list and the given lists.
   * @return A new list after performing the map, then final join.
   */
  public <B, C, D, E> List<E> bind(final List<B> lb, final List<C> lc, final List<D> ld, final F<A, F<B, F<C, F<D, E>>>> f) {
    return ld.apply(bind(lb, lc, f));
  }

  /**
   * Binds the given function across each element of this list and the given lists with a final
   * join.
   *
   * @param lb A given list to bind the given function with.
   * @param lc A given list to bind the given function with.
   * @param ld A given list to bind the given function with.
   * @param le A given list to bind the given function with.
   * @param f  The function to apply to each element of this list and the given lists.
   * @return A new list after performing the map, then final join.
   */
  public <B, C, D, E, F$> List<F$> bind(final List<B> lb, final List<C> lc, final List<D> ld, final List<E> le, final F<A, F<B, F<C, F<D, F<E, F$>>>>> f) {
    return le.apply(bind(lb, lc, ld, f));
  }

  /**
   * Binds the given function across each element of this list and the given lists with a final
   * join.
   *
   * @param lb A given list to bind the given function with.
   * @param lc A given list to bind the given function with.
   * @param ld A given list to bind the given function with.
   * @param le A given list to bind the given function with.
   * @param lf A given list to bind the given function with.
   * @param f  The function to apply to each element of this list and the given lists.
   * @return A new list after performing the map, then final join.
   */
  public <B, C, D, E, F$, G> List<G> bind(final List<B> lb, final List<C> lc, final List<D> ld, final List<E> le, final List<F$> lf, final F<A, F<B, F<C, F<D, F<E, F<F$, G>>>>>> f) {
    return lf.apply(bind(lb, lc, ld, le, f));
  }

  /**
   * Binds the given function across each element of this list and the given lists with a final
   * join.
   *
   * @param lb A given list to bind the given function with.
   * @param lc A given list to bind the given function with.
   * @param ld A given list to bind the given function with.
   * @param le A given list to bind the given function with.
   * @param lf A given list to bind the given function with.
   * @param lg A given list to bind the given function with.
   * @param f  The function to apply to each element of this list and the given lists.
   * @return A new list after performing the map, then final join.
   */
  public <B, C, D, E, F$, G, H> List<H> bind(final List<B> lb, final List<C> lc, final List<D> ld, final List<E> le, final List<F$> lf, final List<G> lg, final F<A, F<B, F<C, F<D, F<E, F<F$, F<G, H>>>>>>> f) {
    return lg.apply(bind(lb, lc, ld, le, lf, f));
  }

  /**
   * Binds the given function across each element of this list and the given lists with a final
   * join.
   *
   * @param lb A given list to bind the given function with.
   * @param lc A given list to bind the given function with.
   * @param ld A given list to bind the given function with.
   * @param le A given list to bind the given function with.
   * @param lf A given list to bind the given function with.
   * @param lg A given list to bind the given function with.
   * @param lh A given list to bind the given function with.
   * @param f  The function to apply to each element of this list and the given lists.
   * @return A new list after performing the map, then final join.
   */
  public <B, C, D, E, F$, G, H, I> List<I> bind(final List<B> lb, final List<C> lc, final List<D> ld, final List<E> le, final List<F$> lf, final List<G> lg, final List<H> lh, final F<A, F<B, F<C, F<D, F<E, F<F$, F<G, F<H, I>>>>>>>> f) {
    return lh.apply(bind(lb, lc, ld, le, lf, lg, f));
  }

  /**
   * Performs a bind across each list element, but ignores the element value each time.
   *
   * @param bs The list to apply in the final join.
   * @return A new list after the final join.
   */
  public <B> List<B> sequence(final List<B> bs) {
    final F<A, List<B>> c = constant(bs);
    return bind(c);
  }

  /**
   * Performs function application within a list (applicative functor pattern).
   *
   * @param lf The list of functions to apply.
   * @return A new list after applying the given list of functions through this list.
   */
  public <B> List<B> apply(final List<F<A, B>> lf) {
    return lf.bind(new F<F<A, B>, List<B>>() {
      public List<B> f(final F<A, B> f) {
        return map(f);
      }
    });
  }

  /**
   * Appends the given list to this list.
   *
   * @param as The list to append to this one.
   * @return A new list that has appended the given list.
   */
  public List<A> append(final List<A> as) {
    return Buffer.fromList(this).append(as).toList();
  }

  /**
   * Performs a right-fold reduction across this list. This function uses O(length) stack space.
   *
   * @param f The function to apply on each element of the list.
   * @param b The beginning value to start the application from.
   * @return The final result after the right-fold reduction.
   */
  public <B> B foldRight(final F<A, F<B, B>> f, final B b) {
    return isEmpty() ? b : f.f(head()).f(tail().foldRight(f, b));
  }

  /**
   * Performs a right-fold reduction across this list. This function uses O(length) stack space.
   *
   * @param f The function to apply on each element of the list.
   * @param b The beginning value to start the application from.
   * @return The final result after the right-fold reduction.
   */
  public <B> B foldRight(final F2<A, B, B> f, final B b) {
    return foldRight(curry(f), b);
  }

  /**
   * Performs a left-fold reduction across this list. This function runs in constant space.
   *
   * @param f The function to apply on each element of the list.
   * @param b The beginning value to start the application from.
   * @return The final result after the left-fold reduction.
   */
  public <B> B foldLeft(final F<B, F<A, B>> f, final B b) {
    B x = b;

    for (List<A> xs = this; !xs.isEmpty(); xs = xs.tail()) {
      x = f.f(x).f(xs.head());
    }

    return x;
  }

  /**
   * Performs a left-fold reduction across this list. This function runs in constant space.
   *
   * @param f The function to apply on each element of the list.
   * @param b The beginning value to start the application from.
   * @return The final result after the left-fold reduction.
   */
  public <B> B foldLeft(final F2<B, A, B> f, final B b) {
    return foldLeft(curry(f), b);
  }

  /**
   * Takes the first 2 elements of the list and applies the function to them,
   * then applies the function to the result and the third element and so on.
   *
   * @param f The function to apply on each element of the list.
   * @return The final result after the left-fold reduction.
   */
  public A foldLeft1(final F2<A, A, A> f) {
    return foldLeft1(curry(f));
  }

  /**
   * Takes the first 2 elements of the list and applies the function to them,
   * then applies the function to the result and the third element and so on.
   *
   * @param f The function to apply on each element of the list.
   * @return The final result after the left-fold reduction.
   */
  public A foldLeft1(final F<A, F<A, A>> f) {
    if (isEmpty())
      throw error("Undefined: foldLeft1 on empty list");
    return tail().foldLeft(f, head());
  }

  /**
   * Reverse this list in constant stack space.
   *
   * @return A new list that is the reverse of this one.
   */
  public List<A> reverse() {
    return foldLeft(new F<List<A>, F<A, List<A>>>() {
      public F<A, List<A>> f(final List<A> as) {
        return new F<A, List<A>>() {
          public List<A> f(final A a) {
            return cons(a, as);
          }
        };
      }
    }, List.<A>nil());
  }

  /**
   * Returns the element at the given index if it exists, fails otherwise.
   *
   * @param i The index at which to get the element to return.
   * @return The element at the given index if it exists, fails otherwise.
   */
  public A index(final int i) {
    if (i < 0 || i > length() - 1)
      throw error("index " + i + " out of range on list with length " + length());
    else {
      List<A> xs = this;

      for (int c = 0; c < i; c++) {
        xs = xs.tail();
      }

      return xs.head();
    }
  }

  /**
   * Takes the given number of elements from the head of this list if they are available.
   *
   * @param i The maximum number of elements to take from this list.
   * @return A new list with a length the same, or less than, this list.
   */
  public List<A> take(final int i) {
    return i <= 0 || isEmpty() ? List.<A>nil() : cons(head(), tail().take(i - 1));
  }

  /**
   * Drops the given number of elements from the head of this list if they are available.
   *
   * @param i The number of elements to drop from the head of this list.
   * @return A list with a length the same, or less than, this list.
   */
  public List<A> drop(final int i) {
    int c = 0;

    List<A> xs = this;

    //noinspection ForLoopWithMissingComponent,StatementWithEmptyBody
    for (; xs.isNotEmpty() && c < i; xs = xs.tail())
      c++;

    return xs;
  }

  /**
   * Splits this list into two lists at the given index. If the index goes out of bounds, then it is
   * normalised so that this function never fails.
   *
   * @param i The index at which to split this list in two parts.
   * @return A pair of lists split at the given index of this list.
   */
  public P2<List<A>, List<A>> splitAt(final int i) {
    P2<List<A>, List<A>> s = p(List.<A>nil(), List.<A>nil());

    int c = 0;
    for (List<A> xs = this; xs.isNotEmpty(); xs = xs.tail()) {
      final A h = xs.head();
      s = c < i ? s.map1(new F<List<A>, List<A>>() {
        public List<A> f(final List<A> as) {
          return as.snoc(h);
        }
      }) : s.map2(new F<List<A>, List<A>>() {
        public List<A> f(final List<A> as) {
          return as.snoc(h);
        }
      });
      c++;
    }

    return s;
  }

  /**
   * Splits this list into lists of the given size. If the size of this list is not evenly divisible by
   * the given number, the last partition will contain the remainder.
   *
   * @param n The size of the partitions into which to split this list.
   * @return A list of sublists of this list, of at most the given size.
   */
  public List<List<A>> partition(final int n) {
    if (n < 1)
      throw error("Can't create list partitions shorter than 1 element long.");
    if (isEmpty())
      throw error("Partition on empty list.");
    return unfold(new F<List<A>, Option<P2<List<A>, List<A>>>>() {
      public Option<P2<List<A>, List<A>>> f(final List<A> as) {
        return as.isEmpty() ? Option.<P2<List<A>, List<A>>>none() : some(as.splitAt(n));
      }
    }, this);
  }

  /**
   * Returns the list of initial segments of this list, shortest first.
   *
   * @return The list of initial segments of this list, shortest first.
   */
  public List<List<A>> inits() {
    List<List<A>> s = single(List.<A>nil());
    if (isNotEmpty())
      s = s.append(tail().inits().map(List.<A>cons().f(head())));
    return s;
  }

  /**
   * Returns the list of final segments of this list, longest first.
   *
   * @return The list of final segments of this list, longest first.
   */
  public List<List<A>> tails() {
    if (isEmpty())
      return single(List.<A>nil());
    else
      return cons(this, tail().tails());
  }

  /**
   * Sorts this list using the given order over elements using a <em>merge sort</em> algorithm.
   *
   * @param o The order over the elements of this list.
   * @return A sorted list according to the given order.
   */
  public List<A> sort(final Ord<A> o) {
    if (isEmpty())
      return nil();
    else if (tail().isEmpty())
      return this;
    else {
      final class Merge<A> {
        List<A> merge(final List<A> xs, final List<A> ys, final Ord<A> o) {
          return xs.isEmpty() ?
              ys :
              ys.isEmpty() ?
                  xs :
                  o.isGreaterThan(ys.head(), xs.head()) ?
                      cons(xs.head(), merge(xs.tail(), ys, o)) :
                      cons(ys.head(), merge(xs, ys.tail(), o));
        }
      }

      final P2<List<A>, List<A>> s = splitAt(length() / 2);
      return new Merge<A>().merge(s._1().sort(o), s._2().sort(o), o);
    }
  }

  /**
   * Zips this list with the given list using the given function to produce a new list. If this list
   * and the given list have different lengths, then the longer list is normalised so this function
   * never fails.
   *
   * @param bs The list to zip this list with.
   * @param f  The function to zip this list and the given list with.
   * @return A new list with a length the same as the shortest of this list and the given list.
   */
  public <B, C> List<C> zipWith(final List<B> bs, final F<A, F<B, C>> f) {
    return isEmpty() || bs.isEmpty() ? List.<C>nil() : cons(f.f(head()).f(bs.head()), tail().zipWith(bs.tail(), f));
  }

  /**
   * Provides a first-class version of zipWith
   *
   * @return The first-class version of zipWith
   */
  public static <A, B, C> F<List<A>, F<List<B>, F<F<A, F<B, C>>, List<C>>>> zipWith() {
    return curry(new F3<List<A>, List<B>, F<A, F<B, C>>, List<C>>() {
      public List<C> f(final List<A> as, final List<B> bs, final F<A, F<B, C>> f) {
        return as.zipWith(bs, f);
      }
    });
  }

  /**
   * Zips this list with the given list to produce a list of pairs. If this list and the given list
   * have different lengths, then the longer list is normalised so this function never fails.
   *
   * @param bs The list to zip this list with.
   * @return A new list with a length the same as the shortest of this list and the given list.
   */
  public <B> List<P2<A, B>> zip(final List<B> bs) {
    final F<A, F<B, P2<A, B>>> __2 = p2();
    return zipWith(bs, __2);
  }

  /**
   * The first-class version of the zip function.
   *
   * @return A function that zips the given lists to produce a list of pairs.
   */
  public static <A, B> F<List<A>, F<List<B>, List<P2<A, B>>>> zip() {
    return curry(new F2<List<A>, List<B>, List<P2<A, B>>>() {
      public List<P2<A, B>> f(final List<A> as, final List<B> bs) {
        return as.zip(bs);
      }
    });
  }

  /**
   * Zips this list with the index of its element as a pair.
   *
   * @return A new list with the same length as this list.
   */
  public List<P2<A, Integer>> zipIndex() {
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
   * Appends (snoc) the given element to this list to produce a new list.
   *
   * @param a The element to append to this list.
   * @return A new list with the given element appended.
   */
  public List<A> snoc(final A a) {
    return Buffer.fromList(this).snoc(a).toList();
  }

  /**
   * Returns <code>true</code> if the predicate holds for all of the elements of this list,
   * <code>false</code> otherwise (<code>true</code> for the empty list).
   *
   * @param f The predicate function to test on each element of this list.
   * @return <code>true</code> if the predicate holds for all of the elements of this list,
   *         <code>false</code> otherwise.
   */
  public boolean forall(final F<A, Boolean> f) {
    return isEmpty() || f.f(head()) && tail().forall(f);
  }

  /**
   * Returns <code>true</code> if the predicate holds for at least one of the elements of this list,
   * <code>false</code> otherwise (<code>false</code> for the empty list).
   *
   * @param f The predicate function to test on the elements of this list.
   * @return <code>true</code> if the predicate holds for at least one of the elements of this
   *         list.
   */
  public boolean exists(final F<A, Boolean> f) {
    return find(f).isSome();
  }

  /**
   * Finds the first occurrence of an element that matches the given predicate or no value if no
   * elements match.
   *
   * @param f The predicate function to test on elements of this list.
   * @return The first occurrence of an element that matches the given predicate or no value if no
   *         elements match.
   */
  public Option<A> find(final F<A, Boolean> f) {
    for (List<A> as = this; as.isNotEmpty(); as = as.tail()) {
      if (f.f(as.head()))
        return some(as.head());
    }

    return none();
  }

  /**
   * Intersperses the given argument between each element of this list.
   *
   * @param a The separator to intersperse in this list.
   * @return A list with the given separator interspersed.
   */
  public List<A> intersperse(final A a) {
    return isEmpty() || tail().isEmpty() ?
        this :
        cons(head(), cons(a, tail().intersperse(a)));
  }

  /**
   * Removes duplicates according to object equality.
   *
   * @return A list without duplicates according to object equality.
   */
  public List<A> nub() {
    return nub(Equal.<A>anyEqual());
  }

  /**
   * Removes duplicates according to the given equality. Warning: O(n^2).
   *
   * @param eq Equality over the elements.
   * @return A list without duplicates.
   */
  public List<A> nub(final Equal<A> eq) {
    return isEmpty() ? this : cons(head(), tail().filter(new F<A, Boolean>() {
      public Boolean f(final A a) {
        return !eq.eq(a, head());
      }
    }).nub(eq));
  }

  /**
   * Returns a new list of all the items in this list that do not appear in the given list.
   *
   * @param eq an equality for the items of the lists.
   * @param xs a list to subtract from this list.
   * @return a list of all the items in this list that do not appear in the given list.
   */
  public List<A> minus(final Equal<A> eq, final List<A> xs) {
    return removeAll(compose(Monoid.disjunctionMonoid.sumLeft(), xs.mapM(curry(eq.eq()))));
  }

  /**
   * Maps the given function of arity-2 across this list and returns a function that applies all the resulting
   * functions to a given argument.
   *
   * @param f A function of arity-2
   * @return A function that, when given an argument, applies the given function to that argument and every element
   *         in this list.
   */
  public <B, C> F<B, List<C>> mapM(final F<A, F<B, C>> f) {
    return List.sequence(map(f));
  }

  /**
   * Projects an immutable collection of this list.
   *
   * @return An immutable collection of this list.
   */
  public Collection<A> toCollection() {
    return new AbstractCollection<A>() {
      public Iterator<A> iterator() {
        return new Iterator<A>() {
          private List<A> xs = List.this;

          public boolean hasNext() {
            return xs.isNotEmpty();
          }

          public A next() {
            if (xs.isEmpty())
              throw new NoSuchElementException();
            else {
              final A a = xs.head();
              xs = xs.tail();
              return a;
            }
          }

          public void remove() {
            throw new UnsupportedOperationException();
          }
        };
      }

      public int size() {
        return length();
      }
    };
  }

  private static final class Nil<A> extends List<A> {
    public A head() {
      throw error("head on empty list");
    }

    public List<A> tail() {
      throw error("tail on empty list");
    }
  }

  private static final class Cons<A> extends List<A> {
    private final A head;
    private List<A> tail;

    Cons(final A head, final List<A> tail) {
      this.head = head;
      this.tail = tail;
    }

    public A head() {
      return head;
    }

    public List<A> tail() {
      return tail;
    }

    private void tail(final List<A> tail) {
      this.tail = tail;
    }
  }

  /**
   * Constructs a list from the given elements.
   *
   * @param as The elements to construct a list with.
   * @return A list with the given elements.
   */
  public static <A> List<A> list(final A... as) {
    return array(as).toList();
  }

  /**
   * Returns an empty list.
   *
   * @return An empty list.
   */
  public static <A> List<A> nil() {
    return new Nil<A>();
  }

  /**
   * Returns a function that prepends (cons) an element to a list to produce a new list.
   *
   * @return A function that prepends (cons) an element to a list to produce a new list.
   */
  public static <A> F<A, F<List<A>, List<A>>> cons() {
    return new F<A, F<List<A>, List<A>>>() {
      public F<List<A>, List<A>> f(final A a) {
        return new F<List<A>, List<A>>() {
          public List<A> f(final List<A> tail) {
            return cons(a, tail);
          }
        };
      }
    };
  }

  /**
   * Returns a function that prepends a value to the given list.
   *
   * @param tail The list to prepend to.
   * @return A function that prepends a value to the given list.
   */
  public static <A> F<A, List<A>> cons(final List<A> tail) {
    return new F<A, List<A>>() {
      public List<A> f(final A a) {
        return tail.cons(a);
      }
    };
  }

  /**
   * Returns a function that prepends the given value to a list.
   *
   * @param a The value to prepend to a list.
   * @return A function that prepends the given value to a list.
   */
  public static <A> F<List<A>, List<A>> cons_(final A a) {
    return new F<List<A>, List<A>>() {
      public List<A> f(final List<A> as) {
        return as.cons(a);
      }
    };
  }

  /**
   * Prepends the given head element to the given tail element to produce a new list.
   *
   * @param head The element to prepend.
   * @param tail The list to prepend to.
   * @return The list with the given element prepended.
   */
  public static <A> List<A> cons(final A head, final List<A> tail) {
    return new Cons<A>(head, tail);
  }

  /**
   * Returns a function that determines whether a given list is empty.
   *
   * @return A function that determines whether a given list is empty.
   */
  public static <A> F<List<A>, Boolean> isEmpty_() {
    return new F<List<A>, Boolean>() {
      public Boolean f(final List<A> as) {
        return as.isEmpty();
      }
    };
  }

  /**
   * Returns a function that determines whether a given list is not empty.
   *
   * @return A function that determines whether a given list is not empty.
   */
  public static <A> F<List<A>, Boolean> isNotEmpty_() {
    return new F<List<A>, Boolean>() {
      public Boolean f(final List<A> as) {
        return as.isNotEmpty();
      }
    };
  }

  /**
   * Joins the given list of lists using a bind operation.
   *
   * @param o The list of lists to join.
   * @return A new list that is the join of the given lists.
   */
  public static <A> List<A> join(final List<List<A>> o) {
    final F<List<A>, List<A>> id = identity();
    return o.bind(id);
  }

  /**
   * A first-class version of join
   *
   * @return A function that joins a list of lists using a bind operation.
   */
  public static <A> F<List<List<A>>, List<A>> join() {
    return new F<List<List<A>>, List<A>>() {
      public List<A> f(final List<List<A>> as) {
        return join(as);
      }
    };
  }

  /**
   * Unfolds across the given function starting at the given value to produce a list.
   *
   * @param f The function to unfold across.
   * @param b The start value to begin the unfold.
   * @return A new list that is a result of unfolding until the function does not produce a value.
   */
  public static <A, B> List<A> unfold(final F<B, Option<P2<A, B>>> f, final B b) {
    Buffer<A> buf = Buffer.empty();
    for (Option<P2<A, B>> o = f.f(b); o.isSome(); o = f.f(o.some()._2())) {
      buf = buf.snoc(o.some()._1());
    }
    return buf.toList();
  }

  /**
   * Transforms a list of pairs into a list of first components and a list of second components.
   *
   * @param xs The list of pairs to transform.
   * @return A list of first components and a list of second components.
   */
  public static <A, B> P2<List<A>, List<B>> unzip(final List<P2<A, B>> xs) {
    Buffer<A> ba = Buffer.empty();
    Buffer<B> bb = Buffer.empty();
    for (final P2<A, B> p : xs) {
      ba = ba.snoc(p._1());
      bb = bb.snoc(p._2());
    }
    return P.p(ba.toList(), bb.toList());
  }

  /**
   * Returns a list of the given value replicated the given number of times.
   *
   * @param n The number of times to replicate the given value.
   * @param a The value to replicate.
   * @return A list of the given value replicated the given number of times.
   */
  public static <A> List<A> replicate(final int n, final A a) {
    return n <= 0 ? List.<A>nil() : replicate(n - 1, a).cons(a);
  }

  /**
   * Returns a list of integers from the given <code>from</code> value (inclusive) to the given
   * <code>to</code> value (exclusive).
   *
   * @param from The minimum value for the list (inclusive).
   * @param to   The maximum value for the list (exclusive).
   * @return A list of integers from the given <code>from</code> value (inclusive) to the given
   *         <code>to</code> value (exclusive).
   */
  public static List<Integer> range(final int from, final int to) {
    return from >= to ? List.<Integer>nil() : cons(from, range(from + 1, to));
  }

  /**
   * Returns a list of characters from the given string. The inverse of this function is {@link
   * #asString(List)}.
   *
   * @param s The string to produce the list of characters from.
   * @return A list of characters from the given string.
   */
  public static List<Character> fromString(final String s) {
    List<Character> cs = nil();

    for (int i = s.length() - 1; i >= 0; i--)
      cs = cons(s.charAt(i), cs);

    return cs;
  }

  /**
   * A first-class <code>fromString</code>.
   *
   * @return A first-class <code>fromString</code>.
   */
  public static F<String, List<Character>> fromString() {
    return new F<String, List<Character>>() {
      public List<Character> f(final String s) {
        return fromString(s);
      }
    };
  }

  /**
   * Returns a string from the given list of characters. The invers of this function is {@link
   * #fromString(String)}.
   *
   * @param cs The list of characters to produce the string from.
   * @return A string from the given list of characters.
   */
  public static String asString(final List<Character> cs) {
    final StringBuilder sb = new StringBuilder();

    cs.foreach(new F<Character, Unit>() {
      public Unit f(final Character c) {
        sb.append(c);
        return unit();
      }
    });
    return sb.toString();
  }

  /**
   * A first-class <code>asString</code>.
   *
   * @return A first-class <code>asString</code>.
   */
  public static F<List<Character>, String> asString() {
    return new F<List<Character>, String>() {
      public String f(final List<Character> cs) {
        return asString(cs);
      }
    };
  }

  /**
   * Returns a list of one element containing the given value.
   *
   * @param a The value for the head of the returned list.
   * @return A list of one element containing the given value.
   */
  public static <A> List<A> single(final A a) {
    return cons(a, List.<A>nil());
  }

  /**
   * Creates a list where the first item is calculated by applying the function on the third argument,
   * the second item by applying the function on the previous result and so on.
   *
   * @param f The function to iterate with.
   * @param p The predicate which must be true for the next item in order to continue the iteration.
   * @param a The input to the first iteration.
   * @return A list where the first item is calculated by applying the function on the third argument,
   *         the second item by applying the function on the previous result and so on.
   */
  public static <A> List<A> iterateWhile(final F<A, A> f, final F<A, Boolean> p, final A a) {
    return unfold(
        new F<A, Option<P2<A, A>>>() {
          public Option<P2<A, A>> f(final A o) {
            return Option.iif(new F<P2<A, A>, Boolean>() {
              public Boolean f(final P2<A, A> p2) {
                return p.f(o);
              }
            }, P.p(o, f.f(o)));
          }
        }
        , a);
  }

  /**
   * Returns an associated value with the given key in the list of pairs.
   *
   * @param e The test for equality on keys.
   * @param x The list of pairs to search.
   * @param a The key value to find the associated value of.
   * @return An associated value with the given key in the list of pairs.
   */
  public static <A, B> Option<B> lookup(final Equal<A> e, final List<P2<A, B>> x, final A a) {
    return x.find(new F<P2<A, B>, Boolean>() {
      public Boolean f(final P2<A, B> p) {
        return e.eq(p._1(), a);
      }
    }).map(P2.<A, B>__2());
  }

  /**
   * Returns a partially applied version of {@link #lookup(Equal, List, Object)}.
   *
   * @param e The test for equality on keys.
   * @return A partially applied version of {@link #lookup(Equal, List, Object)}.
   */
  public static <A, B> F2<List<P2<A, B>>, A, Option<B>> lookup(final Equal<A> e) {
    return new F2<List<P2<A, B>>, A, Option<B>>() {
      public Option<B> f(final List<P2<A, B>> x, final A a) {
        return lookup(e, x, a);
      }
    };
  }

  /**
   * Provides a first-class version of bind()
   *
   * @return The bind function for lists.
   */
  public static <A, B> F<F<A, List<B>>, F<List<A>, List<B>>> bind_() {
    return curry(new F2<F<A, List<B>>, List<A>, List<B>>() {
      public List<B> f(final F<A, List<B>> f, final List<A> as) {
        return as.bind(f);
      }
    });
  }

  /**
   * Provides a first-class version of map()
   *
   * @return The map function for lists.
   */
  public static <A, B> F<F<A, B>, F<List<A>, List<B>>> map_() {
    return curry(new F2<F<A, B>, List<A>, List<B>>() {
      public List<B> f(final F<A, B> f, final List<A> as) {
        return as.map(f);
      }
    });
  }

  /**
   * Turn a list of functions into a function returning a list.
   *
   * @param fs The list of functions to sequence into a single function that returns a list.
   * @return A function that, when given an argument, applies all the functions in the given list to it
   *         and returns a list of the results.
   */
  public static <A, B> F<B, List<A>> sequence(final List<F<B, A>> fs) {
    return fs.foldRight(fj.Function.<A, List<A>, List<A>, B>lift(List.<A>cons()), fj.Function
        .<B, List<A>>constant(List.<A>nil()));
  }

  /**
   * Provides a first-class version of foldLeft.
   *
   * @return The left fold function for lists.
   */
  public static <A, B> F<F<B, F<A, B>>, F<B, F<List<A>, B>>> foldLeft() {
    return curry(new F3<F<B, F<A, B>>, B, List<A>, B>() {
      public B f(final F<B, F<A, B>> f, final B b, final List<A> as) {
        return as.foldLeft(f, b);
      }
    });
  }

  /**
   * Takes the given iterable to a list.
   *
   * @param i The iterable to take to a list.
   * @return A list from the given iterable.
   */
  public static <A> List<A> iterableList(final Iterable<A> i) {
    final Buffer<A> bs = Buffer.empty();

    for (final A a : i)
      bs.snoc(a);

    return bs.toList();
  }

  /**
   * A mutable, singly linked list. This structure should be used <em>very</em> sparingly, in favour
   * of the {@link List immutable singly linked list structure}.
   */
  public static final class Buffer<A> implements Iterable<A> {
    private List<A> start = nil();
    private Cons<A> tail;
    private boolean exported;

    /**
     * Returns an iterator for this buffer. This method exists to permit the use in a <code>for</code>-each loop.
     *
     * @return A iterator for this buffer.
     */
    public Iterator<A> iterator() {
      return start.iterator();
    }

    /**
     * Appends (snoc) the given element to this buffer to produce a new buffer.
     *
     * @param a The element to append to this buffer.
     * @return A new buffer with the given element appended.
     */
    public Buffer<A> snoc(final A a) {
      if (exported)
        copy();

      final Cons<A> t = new Cons<A>(a, List.<A>nil());

      if (tail == null)
        start = t;
      else
        tail.tail(t);

      tail = t;

      return this;
    }

    /**
     * Appends the given buffer to this buffer.
     *
     * @param as The buffer to append to this one.
     * @return A new buffer that has appended the given buffer.
     */
    public Buffer<A> append(final List<A> as) {
      for (List<A> xs = as; xs.isNotEmpty(); xs = xs.tail())
        snoc(xs.head());

      return this;
    }

    /**
     * Returns an immutable list projection of this buffer. Modifications to the underlying buffer
     * will <em>not</em> be reflected in returned lists.
     *
     * @return An immutable list projection of this buffer.
     */
    public List<A> toList() {
      exported = !start.isEmpty();
      return start;
    }

    /**
     * Projects an immutable collection of this buffer.
     *
     * @return An immutable collection of this buffer.
     */
    public Collection<A> toCollection() {
      return start.toCollection();
    }

    /**
     * An empty buffer.
     *
     * @return An empty buffer.
     */
    public static <A> Buffer<A> empty() {
      return new Buffer<A>();
    }

    /**
     * Constructs a buffer from the given list.
     *
     * @param as The list to construct a buffer with.
     * @return A buffer from the given list.
     */
    public static <A> Buffer<A> fromList(final List<A> as) {
      final Buffer<A> b = new Buffer<A>();

      for (List<A> xs = as; xs.isNotEmpty(); xs = xs.tail())
        b.snoc(xs.head());

      return b;
    }

    /**
     * Takes the given iterable to a buffer.
     *
     * @param i The iterable to take to a buffer.
     * @return A buffer from the given iterable.
     */
    public static <A> Buffer<A> iterableBuffer(final Iterable<A> i) {
      final Buffer<A> b = Buffer.empty();

      for (final A a : i)
        b.snoc(a);

      return b;
    }

    private void copy() {
      List<A> s = start;
      final Cons<A> t = tail;
      start = nil();
      exported = false;
      while (s != t) {
        snoc(s.head());
        s = s.tail();
      }

      if (t != null)
        snoc(t.head());
    }
  }
}
