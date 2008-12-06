package fj.data;

import static fj.Bottom.error;
import fj.Effect;
import fj.F;
import static fj.Function.identity;
import static fj.P.p;
import fj.P1;
import fj.Unit;
import static fj.Unit.unit;
import static fj.data.Array.array;
import static fj.data.List.single;
import static fj.data.List.cons_;
import static fj.data.Option.some;

import java.util.Collection;
import java.util.Iterator;

/**
 * The <code>Either</code> type represents a value of one of two possible types (a disjoint union).
 * The data constructors; <code>Left</code> and <code>Right</code> represent the two possible
 * values. The <code>Either</code> type is often used as an alternative to
 * <code>scala.Option</code> where <code>Left</code> represents failure (by convention) and
 * <code>Right</code> is akin to <code>Some</code>.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public abstract class Either<A, B> {
  private Either() {

  }

  /**
   * Projects this either as a left.
   *
   * @return A left projection of this either.
   */
  public LeftProjection<A, B> left() {
    return new LeftProjection<A, B>(this);
  }

  /**
   * Projects this either as a right.
   *
   * @return A right projection of this either.
   */
  public RightProjection<A, B> right() {
    return new RightProjection<A, B>(this);
  }

  /**
   * Returns <code>true</code> if this either is a left, <code>false</code> otherwise.
   *
   * @return <code>true</code> if this either is a left, <code>false</code> otherwise.
   */
  public abstract boolean isLeft();

  /**
   * Returns <code>true</code> if this either is a right, <code>false</code> otherwise.
   *
   * @return <code>true</code> if this either is a right, <code>false</code> otherwise.
   */
  public abstract boolean isRight();

  /**
   * The catamorphism for either. Folds over this either breaking into left or right.
   *
   * @param left The function to call if this is left.
   * @param right The function to call if this is right.
   * @return The reduced value.
   */
  public <X> X either(final F<A, X> left, final F<B, X> right) {
    return isLeft() ?
            left.f(left().value()) :
            right.f(right().value());
  }

  /**
   * If this is a left, then return the left value in right, or vice versa.
   *
   * @return The value of this either swapped to the opposing side.
   */
  public Either<B, A> swap() {
    return isLeft() ? new Right<B, A>(((Left<A, B>) this).a) : new Left<B, A>(((Right<A, B>) this).b);
  }

  private static final class Left<A, B> extends Either<A, B> {
    private final A a;

    Left(final A a) {
      this.a = a;
    }

    public boolean isLeft() {
      return true;
    }

    public boolean isRight() {
      return false;
    }
  }

  private static final class Right<A, B> extends Either<A, B> {
    private final B b;

    Right(final B b) {
      this.b = b;
    }

    public boolean isLeft() {
      return false;
    }

    public boolean isRight() {
      return true;
    }
  }

  /**
   * A left projection of an either value.
   */
  public final class LeftProjection<A, B> implements Iterable<A> {
    private final Either<A, B> e;

    private LeftProjection(final Either<A, B> e) {
      this.e = e;
    }

    /**
     * Returns an iterator for this projection. This method exists to permit the use in a <code>for</code>-each loop.
     *
     * @return A iterator for this projection.
     */
    public Iterator<A> iterator() {
      return toCollection().iterator();
    }

    /**
     * The either value underlying this projection.
     *
     * @return The either value underlying this projection.
     */
    public Either<A, B> either() {
      return e;
    }

    /**
     * Returns the value of this projection or fails with the given error message.
     *
     * @param err The error message to fail with.
     * @return The value of this projection
     */
    public A valueE(final P1<String> err) {
      if (e.isLeft())
        //noinspection CastToConcreteClass
        return ((Left<A, B>) e).a;
      else
        throw error(err._1());
    }

    /**
     * Returns the value of this projection or fails with the given error message.
     *
     * @param err The error message to fail with.
     * @return The value of this projection
     */
    public A valueE(final String err) {
      return valueE(p(err));
    }

    /**
     * The value of this projection or fails with a specialised error message.
     *
     * @return The value of this projection.
     */
    public A value() {
      return valueE(p("left.value on Right"));
    }

    /**
     * The value of this projection or the given argument.
     *
     * @param a The value to return if this projection has no value.
     * @return The value of this projection or the given argument.
     */
    public A orValue(final P1<A> a) {
      return isLeft() ? value() : a._1();
    }

    /**
     * The value of this projection or the given argument.
     *
     * @param a The value to return if this projection has no value.
     * @return The value of this projection or the given argument.
     */
    public A orValue(final A a) {
      return isLeft() ? value() : a;
    }

    /**
     * The value of this projection or the result of the given function on the opposing projection's
     * value.
     *
     * @param f The function to execute if this projection has no value.
     * @return The value of this projection or the result of the given function on the opposing projection's
     *         value.
     */
    public A on(final F<B, A> f) {
      return isLeft() ? value() : f.f(e.right().value());
    }

    /**
     * Execute a side-effect on this projection's value if it has one.
     *
     * @param f The side-effect to execute.
     * @return The unit value.
     */
    public Unit foreach(final F<A, Unit> f) {
      if (isLeft())
        f.f(value());

      return unit();
    }

    /**
     * Execute a side-effect on this projection's value if it has one.
     *
     * @param f The side-effect to execute.
     */
    public void foreach(final Effect<A> f) {
      if (isLeft())
        f.e(value());
    }

    /**
     * Map the given function across this projection's value if it has one.
     *
     * @param f The function to map across this projection.
     * @return A new either value after mapping.
     */
    public <X> Either<X, B> map(final F<A, X> f) {
      return isLeft() ? new Left<X, B>(f.f(value())) : new Right<X, B>(e.right().value());
    }

    /**
     * Binds the given function across this projection's value if it has one.
     *
     * @param f The function to bind across this projection.
     * @return A new either value after binding.
     */
    public <X> Either<X, B> bind(final F<A, Either<X, B>> f) {
      return isLeft() ? f.f(value()) : new Right<X, B>(e.right().value());
    }

    /**
     * Anonymous bind through this projection.
     *
     * @param e The value to bind with.
     * @return An either after binding through this projection.
     */
    public <X> Either<X, B> sequence(final Either<X, B> e) {
      return bind(fj.Function.<A, Either<X, B>>constant(e));
    }

    /**
     * Returns <code>None</code> if this projection has no value or if the given predicate
     * <code>p</code> does not hold for the value, otherwise, returns a right in <code>Some</code>.
     *
     * @param f The predicate function to test on this projection's value.
     * @return <code>None</code> if this projection has no value or if the given predicate
     *         <code>p</code> does not hold for the value, otherwise, returns a right in <code>Some</code>.
     */
    public <X> Option<Either<A, X>> filter(final F<A, Boolean> f) {
      return isLeft() ?
          f.f(value()) ?
              Option.<Either<A, X>>some(new Left<A, X>(value())) :
              Option.<Either<A, X>>none() :
          Option.<Either<A, X>>none();
    }

    /**
     * Function application on this projection's value.
     *
     * @param e The either of the function to apply on this projection's value.
     * @return The result of function application within either.
     */
    public <X> Either<X, B> apply(final Either<F<A, X>, B> e) {
      return e.left().bind(new F<F<A, X>, Either<X, B>>() {
        public Either<X, B> f(final F<A, X> f) {
          return map(f);
        }
      });
    }

    /**
     * Returns <code>true</code> if no value or returns the result of the application of the given
     * function to the value.
     *
     * @param f The predicate function to test on this projection's value.
     * @return <code>true</code> if no value or returns the result of the application of the given
     *         function to the value.
     */
    public boolean forall(final F<A, Boolean> f) {
      return isRight() || f.f(value());
    }

    /**
     * Returns <code>false</code> if no value or returns the result of the application of the given
     * function to the value.
     *
     * @param f The predicate function to test on this projection's value.
     * @return <code>false</code> if no value or returns the result of the application of the given
     *         function to the value.
     */
    public boolean exists(final F<A, Boolean> f) {
      return isLeft() && f.f(value());
    }

    /**
     * Returns a single element list if this projection has a value, otherwise an empty list.
     *
     * @return A single element list if this projection has a value, otherwise an empty list.
     */
    public List<A> toList() {
      return isLeft() ? single(value()) : List.<A>nil();
    }

    /**
     * Returns this projection's value in <code>Some</code> if it exists, otherwise
     * <code>None</code>.
     *
     * @return This projection's value in <code>Some</code> if it exists, otherwise
     *         <code>None</code>.
     */
    public Option<A> toOption() {
      return isLeft() ? some(value()) : Option.<A>none();
    }

    /**
     * Returns a single element array if this projection has a value, otherwise an empty array.
     *
     * @return A single element array if this projection has a value, otherwise an empty array.
     */
    @SuppressWarnings({"unchecked"})
    public Array<A> toArray() {
      return isLeft() ? array(value()) : Array.<A>empty();
    }

    /**
     * Returns a single element array if this projection has a value, otherwise an empty array.
     *
     * @param c The class type of the array to return.
     * @return A single element array if this projection has a value, otherwise an empty array.
     */
    @SuppressWarnings({"unchecked"})
    public Array<A> toArray(final Class<A[]> c) {
      if (isLeft()) {
        final A[] a = (A[])java.lang.reflect.Array.newInstance(c.getComponentType(), 1);
        a[0] = value();
        return array(a);
      } else
        return array((A[])java.lang.reflect.Array.newInstance(c.getComponentType(), 0));
    }

    /**
     * Returns a single element stream if this projection has a value, otherwise an empty stream.
     *
     * @return A single element stream if this projection has a value, otherwise an empty stream.
     */
    public Stream<A> toStream() {
      return isLeft() ? Stream.single(value()) : Stream.<A>nil();
    }

    /**
     * Projects an immutable collection of this projection.
     *
     * @return An immutable collection of this projection.
     */
    public Collection<A> toCollection() {
      return toList().toCollection();
    }
  }

  /**
   * A right projection of an either value.
   */
  public final class RightProjection<A, B> implements Iterable<B> {
    private final Either<A, B> e;

    private RightProjection(final Either<A, B> e) {
      this.e = e;
    }

    /**
     * Returns an iterator for this projection. This method exists to permit the use in a <code>for</code>-each loop.
     *
     * @return A iterator for this projection.
     */
    public Iterator<B> iterator() {
      return toCollection().iterator();
    }

    /**
     * The either value underlying this projection.
     *
     * @return The either value underlying this projection.
     */
    public Either<A, B> either() {
      return e;
    }

    /**
     * Returns the value of this projection or fails with the given error message.
     *
     * @param err The error message to fail with.
     * @return The value of this projection
     */
    public B valueE(final P1<String> err) {
      if (e.isRight())
        //noinspection CastToConcreteClass
        return ((Right<A, B>) e).b;
      else
        throw error(err._1());
    }

    /**
     * The value of this projection or fails with a specialised error message.
     *
     * @return The value of this projection.
     */
    public B value() {
      return valueE(p("right.value on Left"));
    }

    /**
     * The value of this projection or the given argument.
     *
     * @param b The value to return if this projection has no value.
     * @return The value of this projection or the given argument.
     */
    public B orValue(final P1<B> b) {
      return isRight() ? value() : b._1();
    }

    /**
     * The value of this projection or the result of the given function on the opposing projection's
     * value.
     *
     * @param f The function to execute if this projection has no value.
     * @return The value of this projection or the result of the given function on the opposing projection's
     *         value.
     */
    public B on(final F<A, B> f) {
      return isRight() ? value() : f.f(e.left().value());
    }

    /**
     * Execute a side-effect on this projection's value if it has one.
     *
     * @param f The side-effect to execute.
     * @return The unit value.
     */
    public Unit foreach(final F<B, Unit> f) {
      if (isRight())
        f.f(value());

      return unit();
    }

    /**
     * Execute a side-effect on this projection's value if it has one.
     *
     * @param f The side-effect to execute.
     */
    public void foreach(final Effect<B> f) {
      if (isRight())
        f.e(value());
    }

    /**
     * Map the given function across this projection's value if it has one.
     *
     * @param f The function to map across this projection.
     * @return A new either value after mapping.
     */
    public <X> Either<A, X> map(final F<B, X> f) {
      return isRight() ? new Right<A, X>(f.f(value())) : new Left<A, X>(e.left().value());
    }

    /**
     * Binds the given function across this projection's value if it has one.
     *
     * @param f The function to bind across this projection.
     * @return A new either value after binding.
     */
    public <X> Either<A, X> bind(final F<B, Either<A, X>> f) {
      return isRight() ? f.f(value()) : new Left<A, X>(e.left().value());
    }


    /**
     * Anonymous bind through this projection.
     *
     * @param e The value to bind with.
     * @return An either after binding through this projection.
     */
    public <X> Either<A, X> sequence(final Either<A, X> e) {
      return bind(fj.Function.<B, Either<A, X>>constant(e));
    }

    /**
     * Returns <code>None</code> if this projection has no value or if the given predicate
     * <code>p</code> does not hold for the value, otherwise, returns a left in <code>Some</code>.
     *
     * @param f The predicate function to test on this projection's value.
     * @return <code>None</code> if this projection has no value or if the given predicate
     *         <code>p</code> does not hold for the value, otherwise, returns a left in <code>Some</code>.
     */
    public <X> Option<Either<X, B>> filter(final F<B, Boolean> f) {
      return isRight() ?
          f.f(value()) ?
              Option.<Either<X, B>>some(new Right<X, B>(value())) :
              Option.<Either<X, B>>none() :
          Option.<Either<X, B>>none();
    }

    /**
     * Function application on this projection's value.
     *
     * @param e The either of the function to apply on this projection's value.
     * @return The result of function application within either.
     */
    public <X> Either<A, X> apply(final Either<A, F<B, X>> e) {
      return e.right().bind(new F<F<B, X>, Either<A, X>>() {
        public Either<A, X> f(final F<B, X> f) {
          return map(f);
        }
      });
    }

    /**
     * Returns <code>true</code> if no value or returns the result of the application of the given
     * function to the value.
     *
     * @param f The predicate function to test on this projection's value.
     * @return <code>true</code> if no value or returns the result of the application of the given
     *         function to the value.
     */
    public boolean forall(final F<B, Boolean> f) {
      return isLeft() || f.f(value());
    }

    /**
     * Returns <code>false</code> if no value or returns the result of the application of the given
     * function to the value.
     *
     * @param f The predicate function to test on this projection's value.
     * @return <code>false</code> if no value or returns the result of the application of the given
     *         function to the value.
     */
    public boolean exists(final F<B, Boolean> f) {
      return isRight() && f.f(value());
    }

    /**
     * Returns a single element list if this projection has a value, otherwise an empty list.
     *
     * @return A single element list if this projection has a value, otherwise an empty list.
     */
    public List<B> toList() {
      return isRight() ? single(value()) : List.<B>nil();
    }

    /**
     * Returns this projection's value in <code>Some</code> if it exists, otherwise
     * <code>None</code>.
     *
     * @return This projection's value in <code>Some</code> if it exists, otherwise
     *         <code>None</code>.
     */
    public Option<B> toOption() {
      return isRight() ? some(value()) : Option.<B>none();
    }

    /**
     * Returns a single element array if this projection has a value, otherwise an empty array.
     *
     * @return A single element array if this projection has a value, otherwise an empty array.
     */
    @SuppressWarnings({"unchecked"})
    public Array<B> toArray() {
      return isRight() ? array(value()) : Array.<B>empty();
    }

    /**
     * Returns a single element array if this projection has a value, otherwise an empty array.
     *
     * @param c The class type of the array to return.
     * @return A single element array if this projection has a value, otherwise an empty array.
     */
    @SuppressWarnings({"unchecked"})
    public Array<B> toArray(final Class<B[]> c) {
      if (isRight()) {
        final B[] a = (B[])java.lang.reflect.Array.newInstance(c.getComponentType(), 1);
        a[0] = value();
        return array(a);
      } else
        return array((B[])java.lang.reflect.Array.newInstance(c.getComponentType(), 0));
    }

    /**
     * Returns a single element stream if this projection has a value, otherwise an empty stream.
     *
     * @return A single element stream if this projection has a value, otherwise an empty stream.
     */
    public Stream<B> toStream() {
      return isRight() ? Stream.single(value()) : Stream.<B>nil();
    }

    /**
     * Projects an immutable collection of this projection.
     *
     * @return An immutable collection of this projection.
     */
    public Collection<B> toCollection() {
      return toList().toCollection();
    }
  }

  /**
   * Construct a left value of either.
   *
   * @param a The value underlying the either.
   * @return A left value of either.
   */
  public static <A, B> Either<A, B> left(final A a) {
    return new Left<A, B>(a);
  }

  /**
   * A function that constructs a left value of either.
   *
   * @return A function that constructs a left value of either.
   */
  public static <A, B> F<A, Either<A, B>> left_() {
    return new F<A, Either<A, B>>() {
      public Either<A, B> f(final A a) {
        return left(a);
      }
    };
  }

  /**
   * A function that constructs a right value of either.
   *
   * @return A function that constructs a right value of either.
   */
  public static <A, B> F<B, Either<A, B>> right_() {
    return new F<B, Either<A, B>>() {
      public Either<A, B> f(final B b) {
        return right(b);
      }
    };
  }

  /**
   * Construct a right value of either.
   *
   * @param b The value underlying the either.
   * @return A right value of either.
   */
  public static <A, B> Either<A, B> right(final B b) {
    return new Right<A, B>(b);
  }

  /**
   * @return A function that maps another function across an either's left projection.
   */
  public static <A, B, X> F<F<A, X>, F<Either<A, B>, Either<X, B>>> leftMap_() {
    return new F<F<A, X>, F<Either<A, B>, Either<X, B>>>() {
      public F<Either<A, B>, Either<X, B>> f(final F<A, X> axf) {
        return new F<Either<A, B>, Either<X, B>>() {
          public Either<X, B> f(final Either<A, B> e) {
            return e.left().map(axf);
          }
        };
      }
    };
  }

  /**
   * @return A function that maps another function across an either's right projection.
   */
  public static <A, B, X> F<F<B, X>, F<Either<A, B>, Either<A, X>>> rightMap_() {
    return new F<F<B, X>, F<Either<A, B>, Either<A, X>>>() {
      public F<Either<A, B>, Either<A, X>> f(final F<B, X> axf) {
        return new F<Either<A, B>, Either<A, X>>() {
          public Either<A, X> f(final Either<A, B> e) {
            return e.right().map(axf);
          }
        };
      }
    };
  }

  /**
   * Joins an either through left.
   *
   * @param e The either of either to join.
   * @return An either after joining.
   */
  public static <A, B> Either<A, B> joinLeft(final Either<Either<A, B>, B> e) {
    final F<Either<A, B>, Either<A, B>> id = identity();
    return e.left().bind(id);
  }

  /**
   * Joins an either through right.
   *
   * @param e The either of either to join.
   * @return An either after joining.
   */
  public static <A, B> Either<A, B> joinRight(final Either<A, Either<A, B>> e) {
    final F<Either<A, B>, Either<A, B>> id = identity();
    return e.right().bind(id);
  }

  /**
   * Sequences through the left side of the either monad with a list of values.
   *
   * @param a The list of values to sequence with the either monad.
   * @return A sequenced value.
   */
  public static <A, X> Either<List<A>, X> sequenceLeft(final List<Either<A, X>> a) {
    return a.isEmpty() ?
            Either.<List<A>, X>left(List.<A>nil()) :
            a.head().left().bind(new F<A, Either<List<A>, X>>() {
              public Either<List<A>, X> f(final A aa) {
                return sequenceLeft(a.tail()).left().map(cons_(aa));
              }
            });
  }

  /**
   * Sequences through the right side of the either monad with a list of values.
   *
   * @param a The list of values to sequence with the either monad.
   * @return A sequenced value.
   */
  public static <B, X> Either<X, List<B>> sequenceRight(final List<Either<X, B>> a) {
    return a.isEmpty() ?
            Either.<X, List<B>>right(List.<B>nil()) :
            a.head().right().bind(new F<B, Either<X, List<B>>>() {
              public Either<X, List<B>> f(final B bb) {
                return sequenceRight(a.tail()).right().map(cons_(bb));
              }
            });
  }

  /**
   * Takes an <code>Either</code> to its contained value within left or right.
   *
   * @param e The either to reduce.
   * @return An <code>Either</code> to its contained value within left or right.
   */
  public static <A> A reduce(final Either<A, A> e) {
    return e.isLeft() ? e.left().value() : e.right().value();
  }

  /**
   * If the condition satisfies, return the given A in left, otherwise, return the given B in right.
   *
   * @param c     The condition to test.
   * @param right The right value to use if the condition satisfies.
   * @param left  The left value to use if the condition does not satisfy.
   * @return A constructed either based on the given condition.
   */
  public static <A, B> Either<A, B> iif(final boolean c, final P1<B> right, final P1<A> left) {
    return c ? new Right<A, B>(right._1()) : new Left<A, B>(left._1());
  }

  /**
   * Returns all the left values in the given list.
   *
   * @param es The list of possible left values.
   * @return All the left values in the given list.
   */
  public static <A, B> List<A> lefts(final List<Either<A, B>> es) {
    return es.foldRight(new F<Either<A, B>, F<List<A>, List<A>>>() {
      public F<List<A>, List<A>> f(final Either<A, B> e) {
        return new F<List<A>, List<A>>() {
          public List<A> f(final List<A> as) {
            return e.isLeft() ? as.cons(e.left().value()) : as;
          }
        };
      }
    }, List.<A>nil());
  }

  /**
   * Returns all the right values in the given list.
   *
   * @param es The list of possible right values.
   * @return All the right values in the given list.
   */
  public static <A, B> List<B> rights(final List<Either<A, B>> es) {
    return es.foldRight(new F<Either<A, B>, F<List<B>, List<B>>>() {
      public F<List<B>, List<B>> f(final Either<A, B> e) {
        return new F<List<B>, List<B>>() {
          public List<B> f(final List<B> bs) {
            return e.isRight() ? bs.cons(e.right().value()) : bs;
          }
        };
      }
    }, List.<B>nil());
  }
}

