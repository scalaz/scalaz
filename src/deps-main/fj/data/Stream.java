package fj.data;

import static fj.Bottom.error;
import static fj.Function.constant;
import static fj.Function.curry;
import static fj.Function.identity;
import static fj.Function.compose;
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
import static fj.data.Array.array;
import static fj.data.Option.none;
import static fj.data.Option.some;
import fj.pre.Ordering;
import static fj.pre.Ordering.EQ;
import static fj.pre.Ordering.GT;
import static fj.pre.Ordering.LT;

import java.util.AbstractCollection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * A lazy (not yet evaluated), immutable, singly linked list.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public abstract class Stream<A> implements Iterable<A> {
  private Stream() {

  }

  /**
   * Returns an iterator for this stream. This method exists to permit the use in a <code>for</code>-each loop.
   *
   * @return A iterator for this stream.
   */
  public Iterator<A> iterator() {
    return toCollection().iterator();
  }

  /**
   * The first element of the stream or fails for the empty stream.
   *
   * @return The first element of the stream or fails for the empty stream.
   */
  public abstract A head();

  /**
   * The stream without the first element or fails for the empty stream.
   *
   * @return The stream without the first element or fails for the empty stream.
   */
  public abstract P1<Stream<A>> tail();

  /**
   * Returns <code>true</code> if this stream is empty, <code>false</code> otherwise.
   *
   * @return <code>true</code> if this stream is empty, <code>false</code> otherwise.
   */
  public boolean isEmpty() {
    return this instanceof Nil;
  }

  /**
   * Returns <code>false</code> if this stream is empty, <code>true</code> otherwise.
   *
   * @return <code>false</code> if this stream is empty, <code>true</code> otherwise.
   */
  public boolean isNotEmpty() {
    return this instanceof Cons;
  }

  /**
   * Performs a reduction on this stream using the given arguments.
   *
   * @param nil  The value to return if this stream is empty.
   * @param cons The function to apply to the head and tail of this stream if it is not empty.
   * @return A reduction on this stream.
   */
  public <B> B stream(final B nil, final F<A, F<P1<Stream<A>>, B>> cons) {
    return isEmpty() ? nil : cons.f(head()).f(tail());
  }

  /**
   * Performs a right-fold reduction across this stream. This function uses O(length) stack space.
   *
   * @param f The function to apply on each element of the stream.
   * @param b The beginning value to start the application from.
   * @return The final result after the right-fold reduction.
   */
  public <B> B foldRight(final F<A, F<P1<B>, B>> f, final B b) {
    return isEmpty() ? b : f.f(head()).f(new P1<B>() {
      public B _1() {
        return tail()._1().foldRight(f, b);
      }
    });
  }

  /**
   * Performs a right-fold reduction across this stream. This function uses O(length) stack space.
   *
   * @param f The function to apply on each element of the stream.
   * @param b The beginning value to start the application from.
   * @return The final result after the right-fold reduction.
   */
  public <B> B foldRight(final F2<A, P1<B>, B> f, final B b) {
    return foldRight(curry(f), b);
  }

  /**
   * Performs a right-fold reduction across this stream. This function uses O(length) stack space.
   *
   * @param f The function to apply on each element of the stream.
   * @param b The beginning value to start the application from.
   * @return The final result after the right-fold reduction.
   */
  public <B> B foldRight1(final F<A, F<B, B>> f, final B b) {
    return foldRight(compose(fj.Function.<P1<B>, B, B>andThen().f(P1.<B>__1()), f), b);
  }

  /**
   * Performs a right-fold reduction across this stream. This function uses O(length) stack space.
   *
   * @param f The function to apply on each element of the stream.
   * @param b The beginning value to start the application from.
   * @return The final result after the right-fold reduction.
   */
  public <B> B foldRight1(final F2<A, B, B> f, final B b) {
    return foldRight1(curry(f), b);
  }

  /**
   * Performs a left-fold reduction across this stream. This function runs in constant space.
   *
   * @param f The function to apply on each element of the stream.
   * @param b The beginning value to start the application from.
   * @return The final result after the left-fold reduction.
   */
  public <B> B foldLeft(final F<B, F<A, B>> f, final B b) {
    B x = b;

    for (Stream<A> xs = this; !xs.isEmpty(); xs = xs.tail()._1())
      x = f.f(x).f(xs.head());

    return x;
  }

  /**
   * Performs a left-fold reduction across this stream. This function runs in constant space.
   *
   * @param f The function to apply on each element of the stream.
   * @param b The beginning value to start the application from.
   * @return The final result after the left-fold reduction.
   */
  public <B> B foldLeft(final F2<B, A, B> f, final B b) {
    return foldLeft(curry(f), b);
  }

  /**
   * Returns the head of this stream if there is one or the given argument if this stream is empty.
   *
   * @param a The argument to return if this stream is empty.
   * @return The head of this stream if there is one or the given argument if this stream is empty.
   */
  public A orHead(final P1<A> a) {
    return isEmpty() ? a._1() : head();
  }

  /**
   * Returns the tail of this stream if there is one or the given argument if this stream is empty.
   *
   * @param as The argument to return if this stream is empty.
   * @return The tail of this stream if there is one or the given argument if this stream is empty.
   */
  public P1<Stream<A>> orTail(final P1<Stream<A>> as) {
    return isEmpty() ? as : tail();
  }

  /**
   * Maps the given function across this stream.
   *
   * @param f The function to map across this stream.
   * @return A new stream after the given function has been applied to each element.
   */
  public <B> Stream<B> map(final F<A, B> f) {
    return foldRight(new F<A, F<P1<Stream<B>>, Stream<B>>>() {
      public F<P1<Stream<B>>, Stream<B>> f(final A a) {
        return new F<P1<Stream<B>>, Stream<B>>() {
          public Stream<B> f(final P1<Stream<B>> bs) {
            return cons(f.f(a), bs);
          }
        };
      }
    }, Stream.<B>nil());
  }

  /**
   * Provides a first-class version of the map function.
   *
   * @return A function that maps a given function across a given stream.
   */
  public static <A, B> F<F<A, B>, F<Stream<A>, Stream<B>>> map_() {
    return new F<F<A, B>, F<Stream<A>, Stream<B>>>() {
      public F<Stream<A>, Stream<B>> f(final F<A, B> f) {
        return new F<Stream<A>, Stream<B>>() {
          public Stream<B> f(final Stream<A> as) {
            return as.map(f);
          }
        };
      }
    };
  }

  /**
   * Performs a side-effect for each element of this stream.
   *
   * @param f The side-effect to perform for the given element.
   * @return The unit value.
   */
  public Unit foreach(final F<A, Unit> f) {
    for (Stream<A> xs = this; xs.isNotEmpty(); xs = xs.tail()._1())
      f.f(xs.head());

    return unit();
  }

  /**
   * Performs a side-effect for each element of this stream.
   *
   * @param f The side-effect to perform for the given element.
   */
  public void foreach(final Effect<A> f) {
    for (Stream<A> xs = this; xs.isNotEmpty(); xs = xs.tail()._1())
      f.e(xs.head());
  }

  /**
   * Filters elements from this stream by returning only elements which produce <code>true</code>
   * when the given function is applied to them.
   *
   * @param f The predicate function to filter on.
   * @return A new stream whose elements all match the given predicate.
   */
  public Stream<A> filter(final F<A, Boolean> f) {
    return foldRight(new F<A, F<P1<Stream<A>>, Stream<A>>>() {
      public F<P1<Stream<A>>, Stream<A>> f(final A a) {
        return new F<P1<Stream<A>>, Stream<A>>() {
          public Stream<A> f(final P1<Stream<A>> as) {
            return f.f(a) ? cons(a, as) : as._1();
          }
        };
      }
    }, Stream.<A>nil());
  }

  /**
   * Appends the given stream to this stream.
   *
   * @param as The stream to append to this one.
   * @return A new stream that has appended the given stream.
   */
  public Stream<A> append(final Stream<A> as) {
    return append(P.p(as));
  }

  /**
   * Appends the given stream to this stream.
   *
   * @param as The stream to append to this one.
   * @return A new stream that has appended the given stream.
   */
  public Stream<A> append(final P1<Stream<A>> as) {
    return isEmpty() ? as._1() : cons(head(), new P1<Stream<A>>() {
      public Stream<A> _1() {
        return tail()._1().append(as);
      }
    });
  }

  /**
   * Binds the given function across each element of this stream with a final join.
   *
   * @param f The function to apply to each element of this stream.
   * @return A new stream after performing the map, then final join.
   */
  public <B> Stream<B> bind(final F<A, Stream<B>> f) {
    return foldRight(new F<A, F<P1<Stream<B>>, Stream<B>>>() {
      public F<P1<Stream<B>>, Stream<B>> f(final A a) {
        return new F<P1<Stream<B>>, Stream<B>>() {
          public Stream<B> f(final P1<Stream<B>> bs) {
            return f.f(a).append(bs._1());
          }
        };
      }
    }, Stream.<B>nil());
  }

  /**
   * Binds the given function across each element of this stream and the given stream with a final
   * join.
   *
   * @param sb A given stream to bind the given function with.
   * @param f  The function to apply to each element of this stream and the given stream.
   * @return A new stream after performing the map, then final join.
   */
  public <B, C> Stream<C> bind(final Stream<B> sb, final F<A, F<B, C>> f) {
    return sb.apply(map(f));
  }

  /**
   * Binds the given function across each element of this stream and the given streams with a final
   * join.
   *
   * @param sb A given stream to bind the given function with.
   * @param sc A given stream to bind the given function with.
   * @param f  The function to apply to each element of this stream and the given streams.
   * @return A new stream after performing the map, then final join.
   */
  public <B, C, D> Stream<D> bind(final Stream<B> sb, final Stream<C> sc, final F<A, F<B, F<C, D>>> f) {
    return sc.apply(bind(sb, f));
  }

  /**
   * Binds the given function across each element of this stream and the given streams with a final
   * join.
   *
   * @param sb A given stream to bind the given function with.
   * @param sc A given stream to bind the given function with.
   * @param sd A given stream to bind the given function with.
   * @param f  The function to apply to each element of this stream and the given streams.
   * @return A new stream after performing the map, then final join.
   */
  public <B, C, D, E> Stream<E> bind(final Stream<B> sb, final Stream<C> sc, final Stream<D> sd, final F<A, F<B, F<C, F<D, E>>>> f) {
    return sd.apply(bind(sb, sc, f));
  }

  /**
   * Binds the given function across each element of this stream and the given streams with a final
   * join.
   *
   * @param sb A given stream to bind the given function with.
   * @param sc A given stream to bind the given function with.
   * @param sd A given stream to bind the given function with.
   * @param se A given stream to bind the given function with.
   * @param f  The function to apply to each element of this stream and the given streams.
   * @return A new stream after performing the map, then final join.
   */
  public <B, C, D, E, F$> Stream<F$> bind(final Stream<B> sb, final Stream<C> sc, final Stream<D> sd, final Stream<E> se, final F<A, F<B, F<C, F<D, F<E, F$>>>>> f) {
    return se.apply(bind(sb, sc, sd, f));
  }

  /**
   * Binds the given function across each element of this stream and the given streams with a final
   * join.
   *
   * @param sb A given stream to bind the given function with.
   * @param sc A given stream to bind the given function with.
   * @param sd A given stream to bind the given function with.
   * @param se A given stream to bind the given function with.
   * @param sf A given stream to bind the given function with.
   * @param f  The function to apply to each element of this stream and the given streams.
   * @return A new stream after performing the map, then final join.
   */
  public <B, C, D, E, F$, G> Stream<G> bind(final Stream<B> sb, final Stream<C> sc, final Stream<D> sd, final Stream<E> se, final Stream<F$> sf, final F<A, F<B, F<C, F<D, F<E, F<F$, G>>>>>> f) {
    return sf.apply(bind(sb, sc, sd, se, f));
  }

  /**
   * Binds the given function across each element of this stream and the given streams with a final
   * join.
   *
   * @param sb A given stream to bind the given function with.
   * @param sc A given stream to bind the given function with.
   * @param sd A given stream to bind the given function with.
   * @param se A given stream to bind the given function with.
   * @param sf A given stream to bind the given function with.
   * @param sg A given stream to bind the given function with.
   * @param f  The function to apply to each element of this stream and the given streams.
   * @return A new stream after performing the map, then final join.
   */
  public <B, C, D, E, F$, G, H> Stream<H> bind(final Stream<B> sb, final Stream<C> sc, final Stream<D> sd, final Stream<E> se, final Stream<F$> sf, final Stream<G> sg, final F<A, F<B, F<C, F<D, F<E, F<F$, F<G, H>>>>>>> f) {
    return sg.apply(bind(sb, sc, sd, se, sf, f));
  }

  /**
   * Binds the given function across each element of this stream and the given streams with a final
   * join.
   *
   * @param sb A given stream to bind the given function with.
   * @param sc A given stream to bind the given function with.
   * @param sd A given stream to bind the given function with.
   * @param se A given stream to bind the given function with.
   * @param sf A given stream to bind the given function with.
   * @param sg A given stream to bind the given function with.
   * @param sh A given stream to bind the given function with.
   * @param f  The function to apply to each element of this stream and the given streams.
   * @return A new stream after performing the map, then final join.
   */
  public <B, C, D, E, F$, G, H, I> Stream<I> bind(final Stream<B> sb, final Stream<C> sc, final Stream<D> sd, final Stream<E> se, final Stream<F$> sf, final Stream<G> sg, final Stream<H> sh, final F<A, F<B, F<C, F<D, F<E, F<F$, F<G, F<H, I>>>>>>>> f) {
    return sh.apply(bind(sb, sc, sd, se, sf, sg, f));
  }

  /**
   * Performs a bind across each stream element, but ignores the element value each time.
   *
   * @param bs The stream to apply in the final join.
   * @return A new stream after the final join.
   */
  public <B> Stream<B> sequence(final Stream<B> bs) {
    final F<A, Stream<B>> c = constant(bs);
    return bind(c);
  }

  /**
   * Performs function application within a stream (applicative functor pattern).
   *
   * @param sf The stream of functions to apply.
   * @return A new stream after applying the given stream of functions through this stream.
   */
  public <B> Stream<B> apply(final Stream<F<A, B>> sf) {
    return sf.bind(new F<F<A, B>, Stream<B>>() {
      public Stream<B> f(final F<A, B> f) {
        return map(new F<A, B>() {
          public B f(final A a) {
            return f.f(a);
          }
        });
      }
    });
  }

  /**
   * Interleaves the given stream with this stream to produce a new stream.
   *
   * @param as The stream to interleave this stream with.
   * @return A new stream with elements interleaved from this stream and the given stream.
   */
  public Stream<A> interleave(final Stream<A> as) {
    if (isEmpty())
      return as;
    else if (as.isEmpty())
      return this;
    else {
      return cons(head(), new P1<Stream<A>>() {
        public Stream<A> _1() {
          return cons(as.head(), new P1<Stream<A>>() {
            public Stream<A> _1() {
              return as.tail()._1().interleave(as.tail()._1());
            }
          });
        }
      });
    }
  }

  /**
   * Projects an immutable collection of this stream.
   *
   * @return An immutable collection of this stream.
   */
  public Collection<A> toCollection() {
    return new AbstractCollection<A>() {
      public Iterator<A> iterator() {
        return new Iterator<A>() {
          private Stream<A> xs = Stream.this;

          public boolean hasNext() {
            return xs.isNotEmpty();
          }

          public A next() {
            if (xs.isEmpty())
              throw new NoSuchElementException();
            else {
              final A a = xs.head();
              xs = xs.tail()._1();
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

  /**
   * Returns a stream of integers from the given <code>from</code> value (inclusive) to the given
   * <code>to</code> value (exclusive).
   *
   * @param from The minimum value for the stream (inclusive).
   * @param to   The maximum value for the stream (exclusive).
   * @return A stream of integers from the given <code>from</code> value (inclusive) to the given
   *         <code>to</code> value (exclusive).
   */
  public static Stream<Integer> range(final int from, final int to) {
    return from >= to ? Stream.<Integer>nil() : cons(from, new P1<Stream<Integer>>() {
      public Stream<Integer> _1() {
        return range(from + 1, to);
      }
    });
  }

  /**
   * Returns a stream that is either infinite or bounded up to the maximum value of the given iterator starting at the
   * given value and stepping at increments of <code>1</code>.
   *
   * @param e    The enumerator to compute successors from.
   * @param from The value to begin computing successors from.
   * @return A stream that is either infinite or bounded up to the maximum value of the given iterator starting at the
   *         given value and stepping at increments of <code>1</code>.
   */
  public static <A> Stream<A> forever(final Enumerator<A> e, final A from) {
    return forever(e, from, 1L);
  }

  /**
   * Returns a stream that is either infinite or bounded up to the maximum value of the given iterator starting at the
   * given value and stepping at the given increment.
   *
   * @param e    The enumerator to compute successors from.
   * @param from The value to begin computing successors from.
   * @param step The increment to step.
   * @return A stream that is either infinite or bounded up to the maximum value of the given iterator starting at the
   *         given value and stepping at the given increment.
   */
  public static <A> Stream<A> forever(final Enumerator<A> e, final A from, final long step) {
    return cons(from, new P1<Stream<A>>() {
      public Stream<A> _1() {
        return Stream.join(e.plus(from, step).map(new F<A, Stream<A>>() {
          public Stream<A> f(final A a) {
            return forever(e, a, step);
          }
        }).toStream());
      }
    });
  }

  /**
   * Returns a stream using the given enumerator from the given value to the other given value stepping at increments of
   * <code>1</code>.
   *
   * @param e    The enumerator to compute successors from.
   * @param from The value to begin computing successors from.
   * @param to   The value to stop computing successors from.
   * @return A stream using the given enumerator from the given value to the other given value stepping at increments of
   *         <code>1</code>.
   */
  public static <A> Stream<A> range(final Enumerator<A> e, final A from, final A to) {
    return range(e, from, to, 1L);
  }

  /**
   * Returns a stream using the given enumerator from the given value to the other given value stepping at the given
   * increment.
   *
   * @param e    The enumerator to compute successors from.
   * @param from The value to begin computing successors from.
   * @param to   The value to stop computing successors from.
   * @param step The increment to step.
   * @return A stream using the given enumerator from the given value to the other given value stepping at the given
   *         increment.
   */
  public static <A> Stream<A> range(final Enumerator<A> e, final A from, final A to, final long step) {
    final Ordering o = e.order().compare(from, to);
    if (o == EQ || step > 0L && o == GT || step < 0L && o == LT)
      return single(from);
    else
      return cons(from, new P1<Stream<A>>() {
        public Stream<A> _1() {
          return Stream.join(e.plus(from, step).filter(new F<A, Boolean>() {
            public Boolean f(final A a) {
              return !(o == LT ? e.order().isLessThan(to, a) : e.order().isGreaterThan(to, a));
            }
          }).map(new F<A, Stream<A>>() {
            public Stream<A> f(final A a) {
              return range(e, a, to, step);
            }
          }).toStream());
        }
      });
  }

  /**
   * Returns an infinite stream of integers from the given <code>from</code> value (inclusive).
   *
   * @param from The minimum value for the stream (inclusive).
   * @return A stream of integers from the given <code>from</code> value (inclusive).
   */
  public static Stream<Integer> range(final int from) {
    return cons(from, new P1<Stream<Integer>>() {
      public Stream<Integer> _1() {
        return range(from + 1);
      }
    });
  }

  /**
   * Zips this stream with the given stream of functions, applying each function in turn to the
   * corresponding element in this stream to produce a new stream. If this stream and the given stream
   * have different lengths, then the longer stream is normalised so this function never fails.
   *
   * @param fs The stream of functions to apply to this stream.
   * @return A new stream with a length the same as the shortest of this stream and the given stream.
   */
  public <B> Stream<B> zapp(final Stream<F<A, B>> fs) {
    return fs.isEmpty() || isEmpty() ? Stream.<B>nil() :
      cons(fs.head().f(head()), new P1<Stream<B>>() {
        public Stream<B> _1() {
          return tail()._1().zapp(fs.tail()._1());
        }
      });
  }

  /**
   * Zips this stream with the given stream using the given function to produce a new stream. If
   * this stream and the given stream have different lengths, then the longer stream is normalised
   * so this function never fails.
   *
   * @param bs The stream to zip this stream with.
   * @param f  The function to zip this stream and the given stream with.
   * @return A new stream with a length the same as the shortest of this stream and the given
   *         stream.
   */
  public <B, C> Stream<C> zipWith(final Stream<B> bs, final F<A, F<B, C>> f) {
    return bs.zapp(zapp(repeat(f)));
  }

  /**
   * Zips this stream with the given stream to produce a stream of pairs. If this stream and the
   * given stream have different lengths, then the longer stream is normalised so this function
   * never fails.
   *
   * @param bs The stream to zip this stream with.
   * @return A new stream with a length the same as the shortest of this stream and the given
   *         stream.
   */
  public <B> Stream<P2<A, B>> zip(final Stream<B> bs) {
    final F<A, F<B, P2<A, B>>> __2 = p2();
    return zipWith(bs, __2);
  }

  /**
   * Zips this stream with the index of its element as a pair.
   *
   * @return A new stream with the same length as this stream.
   */
  public Stream<P2<A, Integer>> zipIndex() {
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
   * Returns an either projection of this stream; the given argument in <code>Left</code> if empty,
   * or the first element in <code>Right</code>.
   *
   * @param x The value to return in left if this stream is empty.
   * @return An either projection of this stream.
   */
  public <X> Either<X, A> toEither(final P1<X> x) {
    return isEmpty() ? Either.<X, A>left(x._1()) : Either.<X, A>right(head());
  }

  /**
   * Returns an option projection of this stream; <code>None</code> if empty, or the first element
   * in <code>Some</code>.
   *
   * @return An option projection of this stream.
   */
  public Option<A> toOption() {
    return isEmpty() ? Option.<A>none() : some(head());
  }

  /**
   * Returns a list projection of this stream.
   *
   * @return A list projection of this stream.
   */
  public List<A> toList() {
    List<A> as = List.nil();

    for (Stream<A> x = this; !x.isEmpty(); x = x.tail()._1()) {
      as = as.snoc(x.head());
    }

    return as;
  }

  /**
   * Returns a array projection of this stream.
   *
   * @return A array projection of this stream.
   */
  @SuppressWarnings({"unchecked"})
  public Array<A> toArray() {
    final ArrayList<A> a = new ArrayList<A>();

    for (Stream<A> x = this; x.isNotEmpty(); x = x.tail()._1()) {
      a.add(x.head());
    }

    return array(a.toArray((A[]) new Object[a.size()]));
  }

  /**
   * Returns a array projection of this stream.
   *
   * @param c The class type of the array to return.
   * @return A array projection of this stream.
   */
  @SuppressWarnings({"unchecked"})
  public Array<A> toArray(final Class<A[]> c) {
    final A[] a = (A[]) java.lang.reflect.Array.newInstance(c.getComponentType(), length());

    int i = 0;
    for (final A x : this) {
      a[i] = x;
      i++;
    }

    return array(a);
  }

  /**
   * Prepends (cons) the given element to this stream to product a new stream.
   *
   * @param a The element to prepend.
   * @return A new stream with the given element at the head.
   */
  public Stream<A> cons(final A a) {
    return new Cons<A>(a, new P1<Stream<A>>() {
      public Stream<A> _1() {
        return Stream.this;
      }
    });
  }

  /**
   * Append the given element to this stream to product a new stream.
   *
   * @param a The element to append.
   * @return A new stream with the given element at the end.
   */
  public Stream<A> snoc(final P1<A> a) {
    return append(new P1<Stream<A>>() {
      public Stream<A> _1() {
        return single(a._1());
      }
    });
  }

  /**
   * Returns the first <code>n</code> elements from the head of this stream.
   *
   * @param n The number of elements to take from this stream.
   * @return The first <code>n</code> elements from the head of this stream.
   */
  public Stream<A> take(final int n) {
    return n <= 0 || isEmpty() ?
      Stream.<A>nil() :
      cons(head(), new P1<Stream<A>>() {
        public Stream<A> _1() {
          return tail()._1().take(n - 1);
        }
      });
  }

  /**
   * Drops the given number of elements from the head of this stream if they are available.
   *
   * @param i The number of elements to drop from the head of this stream.
   * @return A stream with a length the same, or less than, this stream.
   */
  public Stream<A> drop(final int i) {
    int c = 0;

    Stream<A> xs = this;

    //noinspection ForLoopWithMissingComponent,StatementWithEmptyBody
    for (; xs.isNotEmpty() && c < i; xs = xs.tail()._1())
      c++;

    return xs;
  }

  /**
   * Returns the first elements of the head of this stream that match the given predicate function.
   *
   * @param f The predicate function to apply on this stream until it finds an element that does not
   *          hold, or the stream is exhausted.
   * @return The first elements of the head of this stream that match the given predicate function.
   */
  public Stream<A> takeWhile(final F<A, Boolean> f) {
    return isEmpty() ?
      this :
      f.f(head()) ?
        cons(head(), new P1<Stream<A>>() {
          public Stream<A> _1() {
            return tail()._1().takeWhile(f);
          }
        }) :
        Stream.<A>nil();
  }

  /**
   * Removes elements from the head of this stream that do not match the given predicate function
   * until an element is found that does match or the stream is exhausted.
   *
   * @param f The predicate function to apply through this stream.
   * @return The stream whose first element does not match the given predicate function.
   */
  public Stream<A> dropWhile(final F<A, Boolean> f) {
    Stream<A> as;
    //noinspection StatementWithEmptyBody
    for (as = this; !as.isEmpty() && f.f(as.head()); as = as.tail()._1()) ;

    return as;
  }

  /**
   * Reverse this stream in constant stack space.
   *
   * @return A new stream that is the reverse of this one.
   */
  public Stream<A> reverse() {
    return foldLeft(new F<Stream<A>, F<A, Stream<A>>>() {
      public F<A, Stream<A>> f(final Stream<A> as) {
        return new F<A, Stream<A>>() {
          public Stream<A> f(final A a) {
            return cons(a, new P1<Stream<A>>() {
              public Stream<A> _1() {
                return as;
              }
            });
          }
        };
      }
    }, Stream.<A>nil());
  }

  /**
   * The length of this stream. This function will not terminate for an infinite stream.
   *
   * @return The length of this stream.
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
   * Returns the element at the given index if it exists, fails otherwise.
   *
   * @param i The index at which to get the element to return.
   * @return The element at the given index if it exists, fails otherwise.
   */
  public A index(final int i) {
    if (i < 0)
      throw error("index " + i + " out of range on stream");
    else {
      Stream<A> xs = this;

      for (int c = 0; c < i; c++) {
        if (xs.isEmpty())
          throw error("index " + i + " out of range on stream");

        xs = xs.tail()._1();
      }

      if (xs.isEmpty())
        throw error("index " + i + " out of range on stream");

      return xs.head();
    }
  }

  /**
   * Returns <code>true</code> if the predicate holds for all of the elements of this stream,
   * <code>false</code> otherwise (<code>true</code> for the empty stream).
   *
   * @param f the predicate function to test on each element of this stream.
   * @return <code>true</code> if the predicate holds for all of the elements of this stream,
   *         <code>false</code> otherwise.
   */
  public boolean forall(final F<A, Boolean> f) {
    return isEmpty() || f.f(head()) && tail()._1().forall(f);
  }

  /**
   * Returns <code>true</code> if the predicate holds for at least one of the elements of this
   * stream, <code>false</code> otherwise (<code>false</code> for the empty stream).
   *
   * @param f The predicate function to test on the elements of this stream.
   * @return <code>true</code> if the predicate holds for at least one of the elements of this
   *         stream.
   */
  public boolean exists(final F<A, Boolean> f) {
    return isNotEmpty() && (f.f(head()) || tail()._1().exists(f));
  }

  /**
   * Finds the first occurrence of an element that matches the given predicate or no value if no
   * elements match.
   *
   * @param f The predicate function to test on elements of this stream.
   * @return The first occurrence of an element that matches the given predicate or no value if no
   *         elements match.
   */
  public Option<A> find(final F<A, Boolean> f) {
    for (Stream<A> as = this; as.isNotEmpty(); as = as.tail()._1()) {
      if (f.f(as.head()))
        return some(as.head());
    }

    return none();
  }

  /**
   * Transforms a stream of pairs into a stream of first components and a stream of second components.
   *
   * @param xs The stream of pairs to transform.
   * @return A stream of first components and a stream of second components.
   */
  public static <A, B> P2<Stream<A>, Stream<B>> unzip(final Stream<P2<A, B>> xs) {
    return xs.foldRight(new F2<P2<A, B>, P1<P2<Stream<A>, Stream<B>>>, P2<Stream<A>, Stream<B>>>() {
      public P2<Stream<A>, Stream<B>> f(final P2<A, B> p, final P1<P2<Stream<A>, Stream<B>>> ps) {
        final P2<Stream<A>, Stream<B>> pp = ps._1();
        return P.p(cons(p._1(), P.p(pp._1())), cons(p._2(), P.p(pp._2())));
      }
    }, P.p(Stream.<A>nil(), Stream.<B>nil()));
  }

  private static final class Nil<A> extends Stream<A> {
    public A head() {
      throw error("head on empty stream");
    }

    public P1<Stream<A>> tail() {
      throw error("tail on empty stream");
    }
  }

  private static final class Cons<A> extends Stream<A> {
    private final A head;
    private final P1<Stream<A>> tail;

    Cons(final A head, final P1<Stream<A>> tail) {
      this.head = head;
      this.tail = tail;
    }

    public A head() {
      return head;
    }

    public P1<Stream<A>> tail() {
      return tail;
    }
  }

  /**
   * Returns a function that prepends (cons) an element to a stream to produce a new stream.
   *
   * @return A function that prepends (cons) an element to a stream to produce a new stream.
   */
  public static <A> F<A, F<P1<Stream<A>>, Stream<A>>> cons() {
    return new F<A, F<P1<Stream<A>>, Stream<A>>>() {
      public F<P1<Stream<A>>, Stream<A>> f(final A a) {
        return new F<P1<Stream<A>>, Stream<A>>() {
          public Stream<A> f(final P1<Stream<A>> list) {
            return cons(a, list);
          }
        };
      }
    };
  }

  /**
   * Returns a function that prepends (cons) an element to a stream to produce a new stream.
   *
   * @return A function that prepends (cons) an element to a stream to produce a new stream.
   */
  public static <A> F<A, F<Stream<A>, Stream<A>>> cons_() {
    return curry(new F2<A, Stream<A>, Stream<A>>() {
      public Stream<A> f(final A a, final Stream<A> as) {
        return as.cons(a);
      }
    });
  }

  /**
   * Returns an empty stream.
   *
   * @return An empty stream.
   */
  public static <A> Stream<A> nil() {
    return new Nil<A>();
  }

  /**
   * Returns an empty stream.
   *
   * @return An empty stream.
   */
  public static <A> P1<Stream<A>> nil_() {
    return new P1<Stream<A>>() {
      public Stream<A> _1() {
        return new Nil<A>();
      }
    };
  }

  /**
   * Returns a function that determines whether a given stream is empty.
   *
   * @return A function that determines whether a given stream is empty.
   */
  public static <A> F<Stream<A>, Boolean> isEmpty_() {
    return new F<Stream<A>, Boolean>() {
      public Boolean f(final Stream<A> as) {
        return as.isEmpty();
      }
    };
  }

  /**
   * Returns a function that determines whether a given stream is not empty.
   *
   * @return A function that determines whether a given stream is not empty.
   */
  public static <A> F<Stream<A>, Boolean> isNotEmpty_() {
    return new F<Stream<A>, Boolean>() {
      public Boolean f(final Stream<A> as) {
        return as.isNotEmpty();
      }
    };
  }

  /**
   * Returns a stream of one element containing the given value.
   *
   * @param a The value for the head of the returned stream.
   * @return A stream of one element containing the given value.
   */
  public static <A> Stream<A> single(final A a) {
    return cons(a, new P1<Stream<A>>() {
      public Stream<A> _1() {
        return nil();
      }
    });
  }

  /**
   * Returns a function that yields a stream containing its argument.
   *
   * @return a function that yields a stream containing its argument.
   */
  public static <A> F<A, Stream<A>> single() {
    return new F<A, Stream<A>>() {
      public Stream<A> f(final A a) {
        return single(a);
      }
    };
  }

  /**
   * Prepends the given head element to the given tail element to produce a new stream.
   *
   * @param head The element to prepend.
   * @param tail The stream to prepend to.
   * @return The stream with the given element prepended.
   */
  public static <A> Stream<A> cons(final A head, final P1<Stream<A>> tail) {
    return new Cons<A>(head, tail);
  }

  /**
   * Joins the given stream of streams using a bind operation.
   *
   * @param o The stream of streams to join.
   * @return A new stream that is the join of the given streams.
   */
  public static <A> Stream<A> join(final Stream<Stream<A>> o) {
    final F<Stream<A>, Stream<A>> id = identity();
    return o.bind(id);
  }

  /**
   * A first-class version of join
   *
   * @return A function that joins a stream of streams using a bind operation.
   */
  public static <A> F<Stream<Stream<A>>, Stream<A>> join() {
    return new F<Stream<Stream<A>>, Stream<A>>() {
      public Stream<A> f(final Stream<Stream<A>> as) {
        return join(as);
      }
    };
  }

  /**
   * Unfolds across the given function starting at the given value to produce a stream.
   *
   * @param f The function to unfold across.
   * @param b The start value to begin the unfold.
   * @return A new stream that is a result of unfolding until the function does not produce a
   *         value.
   */
  public static <A, B> Stream<A> unfold(final F<B, Option<P2<A, B>>> f, final B b) {
    final Option<P2<A, B>> o = f.f(b);
    if (o.isNone())
      return nil();
    else {
      final P2<A, B> p = o.some();
      return cons(p._1(), new P1<Stream<A>>() {
        public Stream<A> _1() {
          return unfold(f, p._2());
        }
      });
    }
  }

  /**
   * Takes the given iterable to a stream.
   *
   * @param i The iterable to take to a stream.
   * @return A stream from the given iterable.
   */
  public static <A> Stream<A> iterableStream(final Iterable<A> i) {
    final class Util {
      public <A> Stream<A> iteratorStream(final Iterator<A> i) {
        if (i.hasNext()) {
          final A a = i.next();
          return cons(a, new P1<Stream<A>>() {
            public Stream<A> _1() {
              return iteratorStream(i);
            }
          });
        } else
          return Stream.nil();
      }
    }

    return new Util().iteratorStream(i.iterator());
  }

  /**
   * Returns an infinite-length stream of the given element.
   *
   * @param a The element to repeat infinitely.
   * @return An infinite-length stream of the given element.
   */
  public static <A> Stream<A> repeat(final A a) {
    return cons(a, new P1<Stream<A>>() {
      public Stream<A> _1() {
        return repeat(a);
      }
    });
  }

  /**
   * Returns a stream constructed by applying the given iteration function starting at the given value.
   *
   * @param f The iteration function.
   * @param a The value to begin iterating from.
   * @return A stream constructed by applying the given iteration function starting at the given value.
   */
  public static <A> Stream<A> iterate(final F<A, A> f, final A a) {
    return cons(a, new P1<Stream<A>>() {
      public Stream<A> _1() {
        return iterate(f, f.f(a));
      }
    });
  }

}
