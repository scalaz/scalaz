package fj.data;

import fj.Effect;
import fj.F;
import fj.F2;
import fj.F3;
import fj.F4;
import fj.F5;
import fj.F6;
import fj.F7;
import fj.F8;
import static fj.Function.curry;
import static fj.P.p;
import fj.P1;
import fj.Unit;
import static fj.Unit.unit;
import static fj.Bottom.error;
import fj.pre.Semigroup;
import java.util.Iterator;

/**
 * Isomorphic to {@link Either} but has renamed functions and represents failure on the left and success on the right.
 * This type also has accumulating functions that accept a {@link Semigroup} for binding computation while keeping error
 * values
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class Validation<E, T> implements Iterable<T> {
  private final Either<E, T> e;

  private Validation(final Either<E, T> e) {
    this.e = e;
  }

  /**
   * Returns <code>true</code> if this is a failure, <code>false</code> otherwise.
   *
   * @return <code>true</code> if this is a failure, <code>false</code> otherwise.
   */
  public boolean isFail() {
    return e.isLeft();
  }

  /**
   * Returns <code>true</code> if this is a success, <code>false</code> otherwise.
   *
   * @return <code>true</code> if this is a success, <code>false</code> otherwise.
   */
  public boolean isSuccess() {
    return e.isRight();
  }

  /**
   * Returns the failing value, or throws an error if there is no failing value.
   *
   * @return the failing value, or throws an error if there is no failing value.
   */
  public E fail() {
    if(isFail())
      return e.left().value();
    else
      throw error("Validation: fail on success value");
  }

  /**
   * Returns the success value, or throws an error if there is no success value.
   *
   * @return the success value, or throws an error if there is no success value.
   */
  public T success() {
    if(isSuccess())
      return e.right().value();
    else
      throw error("Validation: success on fail value");
  }

  /**
   * The catamorphism for validation. Folds over this validation breaking into left or right.
   *
   * @param fail The function to call if this failed.
   * @param success The function to call if this succeeded.
   * @return The reduced value.
   */
  public <X> X validation(final F<E, X> fail, final F<T, X> success) {
    return e.either(fail, success);
  }

  /**
   * Returns a failing projection of this validation.
   *
   * @return a failing projection of this validation.
   */
  public FailProjection<E, T> f() {
    return new FailProjection<E, T>(this);
  }

  /**
   * Returns an either projection of this validation.
   *
   * @return An either projection of this validation.
   */
  public Either<E, T> toEither() {
    return e;
  }

  /**
   * Returns the success value or fails with the given error message.
   *
   * @param err The error message to fail with.
   * @return  The success value.
   */
  public T successE(final P1<String> err) {
    return e.right().valueE(err);
  }

  /**
   * Returns the success value or fails with the given error message.
   *
   * @param err The error message to fail with.
   * @return  The success value.
   */
  public T successE(final String err) {
    return e.right().valueE(p(err));
  }

  /**
   * Returns the success value or the given value.
   *
   * @param t The value to return if this is failure.
   * @return The success value or the given value.
   */
  public T orSuccess(final P1<T> t) {
    return e.right().orValue(t);
  }

  /**
   * Returns the success value or the given value.
   *
   * @param t The value to return if this is failure.
   * @return The success value or the given value.
   */
  public T orSuccess(final T t) {
    return e.right().orValue(p(t));
  }

  /**
   * The success value or the application of the given function to the failing value.
   *
   * @param f The function to execute on the failing value.
   * @return The success value or the application of the given function to the failing value.
   */
  public T on(final F<E, T> f) {
    return e.right().on(f);    
  }

  /**
   * Executes a side-effect on the success value if there is one.
   *
   * @param f The side-effect to execute.
   * @return The unit value.
   */
  public Unit foreach(final F<T, Unit> f) {
    return e.right().foreach(f);
  }

  /**
   * Executes a side-effect on the success value if there is one.
   *
   * @param f The side-effect to execute.
   */
  public void foreach(final Effect<T> f) {
    e.right().foreach(f);
  }

  /**
   * Maps the given function across the success side of this validation.
   *
   * @param f The function to map.
   * @return A new validation with the function mapped.
   */
  @SuppressWarnings({"unchecked"})
  public <A> Validation<E, A> map(final F<T, A> f) {
    return isFail() ?
            Validation.<E, A>fail(fail()) :
            Validation.<E, A>success(f.f(success()));
  }

  /**
   * Binds the given function across this validation's success value if it has one.
   *
   * @param f The function to bind across this validation.
   * @return A new validation value after binding.
   */
  @SuppressWarnings({"unchecked"})
  public <A> Validation<E, A> bind(final F<T, Validation<E, A>> f) {
    return isSuccess() ? f.f(success()) : Validation.<E, A>fail(fail());
  }

  /**
   * Anonymous bind through this validation.
   *
   * @param v The value to bind with.
   * @return A validation after binding.
   */
  public <A> Validation<E, A> sequence(final Validation<E, A> v) {
    return bind(fj.Function.<T, Validation<E, A>>constant(v));
  }

  /**
   * Returns <code>None</code> if this is a failure or if the given predicate <code>p</code> does not hold for the
   * success value, otherwise, returns a success in <code>Some</code>.
   *
   * @param f The predicate function to test on this success value.
   * @return <code>None</code> if this is a failure or if the given predicate <code>p</code> does not hold for the
   * success value, otherwise, returns a success in <code>Some</code>.
   */
  public <A> Option<Validation<A, T>> filter(final F<T, Boolean> f) {
    return e.right().<A>filter(f).map(Validation.<A, T>validation());
  }

  /**
   * Function application on the success value.
   *
   * @param v The validation of the function to apply on the success value.
   * @return The result of function application in validation.
   */
  public <A> Validation<E, A> apply(final Validation<E, F<T, A>> v) {
    return v.bind(new F<F<T, A>, Validation<E, A>>() {
      public Validation<E, A> f(final F<T, A> f) {
        return map(f);
      }
    });
  }

  /**
   * Returns <code>true</code> if this is a failure or returns the result of the application of the given
   * function to the success value.
   *
   * @param f The predicate function to test on this success value.
   * @return <code>true</code> if this is a failure or returns the result of the application of the given
   * function to the success value.
   */
  public boolean forall(final F<T, Boolean> f) {
    return e.right().forall(f);
  }
 
  /**
   * Returns <code>false</code> if this is a failure or returns the result of the application of the given
   * function to the success value.
   *
   * @param f The predicate function to test on this success value.
   * @return <code>false</code> if this is a failure or returns the result of the application of the given
   * function to the success value.
   */
  public boolean exists(final F<T, Boolean> f) {
    return e.right().exists(f);
  }

  /**
   * Returns a single element list if this is a success value, otherwise an empty list.
   *
   * @return A single element list if this is a success value, otherwise an empty list.
   */
  public List<T> toList() {
    return e.right().toList();
  }

  /**
   * Returns the success value in <code>Some</code> if there is one, otherwise <code>None</code>.
   *
   * @return The success value in <code>Some</code> if there is one, otherwise <code>None</code>.
   */
  public Option<T> toOption() {
    return e.right().toOption();
  }

  /**
   * Returns a single element array if this is a success value, otherwise an empty list.
   *
   * @return A single element array if this is a success value, otherwise an empty list.
   */
  @SuppressWarnings({"unchecked"})
  public Array<T> toArray() {
    return e.right().toArray();
  }

  /**
   * Returns a single element array if this is a success value, otherwise an empty list.
   *
   * @param c The class type of the array to return.
   * @return A single element array if this is a success value, otherwise an empty list.
   */
  @SuppressWarnings({"unchecked"})
  public Array<T> toArray(final Class<T[]> c) {
    return e.right().toArray(c);
  }

  /**
   * Returns a single element stream if this is a success value, otherwise an empty list.
   *
   * @return A single element stream if this is a success value, otherwise an empty list.
   */
  public Stream<T> toStream() {
    return e.right().toStream();
  }

  /**
   * Function application on the successful side of this validation, or accumulating the errors on the failing side
   * using the given semigroup should one or more be encountered.
   *
   * @param s The semigroup to accumulate errors with if
   * @param v The validating function to apply.
   * @return A failing validation if this or the given validation failed (with errors accumulated if both) or a
   * succeeding validation if both succeeded.
   */
  @SuppressWarnings({"unchecked"})
  public <A> Validation<E, A> accumapply(final Semigroup<E> s, final Validation<E, F<T, A>> v) {
    return isFail() ?
            Validation.<E, A>fail(v.isFail() ?
                    s.sum(v.fail(), fail()) :
                    fail()) :
            v.isFail() ?
                    Validation.<E, A>fail(v.fail()) :
                    Validation.<E, A>success(v.success().f(success()));
  }

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param f The function to apply if all validations have succeeded.
   * @return A succeeding validation if all validations succeeded, or a failing validation with errors accumulated if
   * one or more failed.
   */
  public <A, B> Validation<E, B> accumulate(final Semigroup<E> s, final Validation<E, A> va, final F<T, F<A, B>> f) {
    return va.accumapply(s, map(f));
  }

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param f The function to apply if all validations have succeeded.
   * @return A succeeding validation if all validations succeeded, or a failing validation with errors accumulated if
   * one or more failed.
   */
  public <A, B> Validation<E, B> accumulate(final Semigroup<E> s, final Validation<E, A> va, final F2<T, A, B> f) {
    return va.accumapply(s, map(curry(f)));
  }

  /**
   * Accumulates errors anonymously.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @return A <code>Some</code> if one or more validations failed (accumulated with the semigroup), otherwise,
   * <code>None</code>.
   */
  public <A> Option<E> accumulate(final Semigroup<E> s, final Validation<E, A> va) {
    return accumulate(s, va, new F2<T, A, Unit>() {
      public Unit f(final T t, final A a) {
        return unit();
      }
    }).f().toOption();
  }

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param f The function to apply if all validations have succeeded.
   * @return A succeeding validation if all validations succeeded, or a failing validation with errors accumulated if
   * one or more failed.
   */
  public <A, B, C> Validation<E, C> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final F<T, F<A, F<B, C>>> f) {
    return vb.accumapply(s, accumulate(s, va, f));
  }

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param f The function to apply if all validations have succeeded.
   * @return A succeeding validation if all validations succeeded, or a failing validation with errors accumulated if
   * one or more failed.
   */
  public <A, B, C> Validation<E, C> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final F3<T, A, B, C> f) {
    return vb.accumapply(s, accumulate(s, va, curry(f)));
  }

  /**
   * Accumulates errors anonymously.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @return A <code>Some</code> if one or more validations failed (accumulated with the semigroup), otherwise,
   * <code>None</code>.
   */
  public <A, B> Option<E> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb) {
    return accumulate(s, va, vb, new F3<T, A, B, Unit>() {
      public Unit f(final T t, final A a, final B b) {
        return unit();
      }
    }).f().toOption();
  }

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param vc The fourth validation to accumulate errors with if it failed.
   * @param f The function to apply if all validations have succeeded.
   * @return A succeeding validation if all validations succeeded, or a failing validation with errors accumulated if
   * one or more failed.
   */
  public <A, B, C, D> Validation<E, D> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final Validation<E, C> vc, final F<T, F<A, F<B, F<C, D>>>> f) {
    return vc.accumapply(s, accumulate(s, va, vb, f));
  }

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param vc The fourth validation to accumulate errors with if it failed.
   * @param f The function to apply if all validations have succeeded.
   * @return A succeeding validation if all validations succeeded, or a failing validation with errors accumulated if
   * one or more failed.
   */
  public <A, B, C, D> Validation<E, D> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final Validation<E, C> vc, final F4<T, A, B, C, D> f) {
    return vc.accumapply(s, accumulate(s, va, vb, curry(f)));
  }

  /**
   * Accumulates errors anonymously.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param vc The fourth validation to accumulate errors with if it failed.
   * @return A <code>Some</code> if one or more validations failed (accumulated with the semigroup), otherwise,
   * <code>None</code>.
   */
  public <A, B, C> Option<E> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final Validation<E, C> vc) {
    return accumulate(s, va, vb, vc, new F4<T, A, B, C, Unit>() {
      public Unit f(final T t, final A a, final B b, final C c) {
        return unit();
      }
    }).f().toOption();
  }

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param vc The fourth validation to accumulate errors with if it failed.
   * @param vd The fifth validation to accumulate errors with if it failed.
   * @param f The function to apply if all validations have succeeded.
   * @return A succeeding validation if all validations succeeded, or a failing validation with errors accumulated if
   * one or more failed.
   */
  public <A, B, C, D, E$> Validation<E, E$> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final Validation<E, C> vc, final Validation<E, D> vd, final F<T, F<A, F<B, F<C, F<D, E$>>>>> f) {
    return vd.accumapply(s, accumulate(s, va, vb, vc, f));
  }

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param vc The fourth validation to accumulate errors with if it failed.
   * @param vd The fifth validation to accumulate errors with if it failed.
   * @param f The function to apply if all validations have succeeded.
   * @return A succeeding validation if all validations succeeded, or a failing validation with errors accumulated if
   * one or more failed.
   */
  public <A, B, C, D, E$> Validation<E, E$> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final Validation<E, C> vc, final Validation<E, D> vd, final F5<T, A, B, C, D, E$> f) {
    return vd.accumapply(s, accumulate(s, va, vb, vc, curry(f)));
  }

  /**
   * Accumulates errors anonymously.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param vc The fourth validation to accumulate errors with if it failed.
   * @param vd The fifth validation to accumulate errors with if it failed.
   * @return A <code>Some</code> if one or more validations failed (accumulated with the semigroup), otherwise,
   * <code>None</code>.
   */
  public <A, B, C, D> Option<E> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final Validation<E, C> vc, final Validation<E, D> vd) {
    return accumulate(s, va, vb, vc, vd, new F5<T, A, B, C, D, Unit>() {
      public Unit f(final T t, final A a, final B b, final C c, final D d) {
        return unit();
      }
    }).f().toOption();
  }

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param vc The fourth validation to accumulate errors with if it failed.
   * @param vd The fifth validation to accumulate errors with if it failed.
   * @param ve The sixth validation to accumulate errors with if it failed.
   * @param f The function to apply if all validations have succeeded.
   * @return A succeeding validation if all validations succeeded, or a failing validation with errors accumulated if
   * one or more failed.
   */
  public <A, B, C, D, E$, F$> Validation<E, F$> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final Validation<E, C> vc, final Validation<E, D> vd, final Validation<E, E$> ve, final F<T, F<A, F<B, F<C, F<D, F<E$, F$>>>>>> f) {
    return ve.accumapply(s, accumulate(s, va, vb, vc, vd, f));
  }

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param vc The fourth validation to accumulate errors with if it failed.
   * @param vd The fifth validation to accumulate errors with if it failed.
   * @param ve The sixth validation to accumulate errors with if it failed.
   * @param f The function to apply if all validations have succeeded.
   * @return A succeeding validation if all validations succeeded, or a failing validation with errors accumulated if
   * one or more failed.
   */
  public <A, B, C, D, E$, F$> Validation<E, F$> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final Validation<E, C> vc, final Validation<E, D> vd, final Validation<E, E$> ve, final F6<T, A, B, C, D, E$, F$> f) {
    return ve.accumapply(s, accumulate(s, va, vb, vc, vd, curry(f)));
  }

  /**
   * Accumulates errors anonymously.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param vc The fourth validation to accumulate errors with if it failed.
   * @param vd The fifth validation to accumulate errors with if it failed.
   * @param ve The sixth validation to accumulate errors with if it failed.
   * @return A <code>Some</code> if one or more validations failed (accumulated with the semigroup), otherwise,
   * <code>None</code>.
   */
  public <A, B, C, D, E$> Option<E> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final Validation<E, C> vc, final Validation<E, D> vd, final Validation<E, E$> ve) {
    return accumulate(s, va, vb, vc, vd, ve, new F6<T, A, B, C, D, E$, Unit>() {
      public Unit f(final T t, final A a, final B b, final C c, final D d, final E$ e) {
        return unit();
      }
    }).f().toOption();
  }

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param vc The fourth validation to accumulate errors with if it failed.
   * @param vd The fifth validation to accumulate errors with if it failed.
   * @param ve The sixth validation to accumulate errors with if it failed.
   * @param vf The seventh validation to accumulate errors with if it failed.
   * @param f The function to apply if all validations have succeeded.
   * @return A succeeding validation if all validations succeeded, or a failing validation with errors accumulated if
   * one or more failed.
   */
  public <A, B, C, D, E$, F$, G> Validation<E, G> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final Validation<E, C> vc, final Validation<E, D> vd, final Validation<E, E$> ve, final Validation<E, F$> vf, final F<T, F<A, F<B, F<C, F<D, F<E$, F<F$, G>>>>>>> f) {
    return vf.accumapply(s, accumulate(s, va, vb, vc, vd, ve, f));
  }

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param vc The fourth validation to accumulate errors with if it failed.
   * @param vd The fifth validation to accumulate errors with if it failed.
   * @param ve The sixth validation to accumulate errors with if it failed.
   * @param vf The seventh validation to accumulate errors with if it failed.
   * @param f The function to apply if all validations have succeeded.
   * @return A succeeding validation if all validations succeeded, or a failing validation with errors accumulated if
   * one or more failed.
   */
  public <A, B, C, D, E$, F$, G> Validation<E, G> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final Validation<E, C> vc, final Validation<E, D> vd, final Validation<E, E$> ve, final Validation<E, F$> vf, final F7<T, A, B, C, D, E$, F$, G> f) {
    return vf.accumapply(s, accumulate(s, va, vb, vc, vd, ve, curry(f)));
  }

  /**
   * Accumulates errors anonymously.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param vc The fourth validation to accumulate errors with if it failed.
   * @param vd The fifth validation to accumulate errors with if it failed.
   * @param ve The sixth validation to accumulate errors with if it failed.
   * @param vf The seventh validation to accumulate errors with if it failed.
   * @return A <code>Some</code> if one or more validations failed (accumulated with the semigroup), otherwise,
   * <code>None</code>.
   */
  public <A, B, C, D, E$, F$> Option<E> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final Validation<E, C> vc, final Validation<E, D> vd, final Validation<E, E$> ve, final Validation<E, F$> vf) {
    return accumulate(s, va, vb, vc, vd, ve, vf, new F7<T, A, B, C, D, E$, F$, Unit>() {
      public Unit f(final T t, final A a, final B b, final C c, final D d, final E$ e, final F$ f) {
        return unit();
      }
    }).f().toOption();
  }

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param vc The fourth validation to accumulate errors with if it failed.
   * @param vd The fifth validation to accumulate errors with if it failed.
   * @param ve The sixth validation to accumulate errors with if it failed.
   * @param vf The seventh validation to accumulate errors with if it failed.
   * @param vg The eighth validation to accumulate errors with if it failed.
   * @param f The function to apply if all validations have succeeded.
   * @return A succeeding validation if all validations succeeded, or a failing validation with errors accumulated if
   * one or more failed.
   */
  public <A, B, C, D, E$, F$, G, H> Validation<E, H> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final Validation<E, C> vc, final Validation<E, D> vd, final Validation<E, E$> ve, final Validation<E, F$> vf, final Validation<E, G> vg, final F<T, F<A, F<B, F<C, F<D, F<E$, F<F$, F<G, H>>>>>>>> f) {
    return vg.accumapply(s, accumulate(s, va, vb, vc, vd, ve, vf, f));
  }

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param vc The fourth validation to accumulate errors with if it failed.
   * @param vd The fifth validation to accumulate errors with if it failed.
   * @param ve The sixth validation to accumulate errors with if it failed.
   * @param vf The seventh validation to accumulate errors with if it failed.
   * @param vg The eighth validation to accumulate errors with if it failed.
   * @param f The function to apply if all validations have succeeded.
   * @return A succeeding validation if all validations succeeded, or a failing validation with errors accumulated if
   * one or more failed.
   */
  public <A, B, C, D, E$, F$, G, H> Validation<E, H> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final Validation<E, C> vc, final Validation<E, D> vd, final Validation<E, E$> ve, final Validation<E, F$> vf, final Validation<E, G> vg, final F8<T, A, B, C, D, E$, F$, G, H> f) {
    return vg.accumapply(s, accumulate(s, va, vb, vc, vd, ve, vf, curry(f)));
  }

  /**
   * Accumulates errors anonymously.
   *
   * @param s The semigroup to accumulate errors with if one or more validations fail.
   * @param va The second validation to accumulate errors with if it failed.
   * @param vb The third validation to accumulate errors with if it failed.
   * @param vc The fourth validation to accumulate errors with if it failed.
   * @param vd The fifth validation to accumulate errors with if it failed.
   * @param ve The sixth validation to accumulate errors with if it failed.
   * @param vf The seventh validation to accumulate errors with if it failed.
   * @param vg The eighth validation to accumulate errors with if it failed.
   * @return A <code>Some</code> if one or more validations failed (accumulated with the semigroup), otherwise, 
   * <code>None</code>.
   */
  public <A, B, C, D, E$, F$, G> Option<E> accumulate(final Semigroup<E> s, final Validation<E, A> va, final Validation<E, B> vb, final Validation<E, C> vc, final Validation<E, D> vd, final Validation<E, E$> ve, final Validation<E, F$> vf, final Validation<E, G> vg) {
    return accumulate(s, va, vb, vc, vd, ve, vf, vg, new F8<T, A, B, C, D, E$, F$, G, Unit>() {
      public Unit f(final T t, final A a, final B b, final C c, final D d, final E$ e, final F$ f, final G g) {
        return unit();
      }
    }).f().toOption();
  }

  /**
   * Returns an iterator for this validation. This method exists to permit the use in a <code>for</code>-each loop.
   *
   * @return A iterator for this validation.
   */
  public Iterator<T> iterator() {
    return toEither().right().iterator();
  }

  /**
   * A failing projection of a validation.
   */
  public final class FailProjection<E, T> implements Iterable<E> {
    private final Validation<E, T> v;

    private FailProjection(final Validation<E, T> v) {
      this.v = v;
    }

    /**
     * Returns the underlying validation.
     *
     * @return The underlying validation.
     */
    public Validation<E, T> validation() {
      return v;
    }

    /**
     * Returns the failing value or fails with the given error message.
     *
     * @param err The error message to fail with.
     * @return  The failing value.
     */
    public E failE(final P1<String> err) {
      return v.toEither().left().valueE(err);
    }

    /**
     * Returns the failing value or fails with the given error message.
     *
     * @param err The error message to fail with.
     * @return  The failing value.
     */
    public E failE(final String err) {
      return failE(p(err));
    }

    /**
     * Returns the failing value or the given value.
     *
     * @param e The value to return if this is success.
     * @return The failing value or the given value.
     */
    public E orFail(final P1<E> e) {
      return v.toEither().left().orValue(e);
    }

    /**
     * Returns the failing value or the given value.
     *
     * @param e The value to return if this is success.
     * @return The failing value or the given value.
     */
    public E orFail(final E e) {
      return orFail(p(e));
    }

    /**
     * The failing value or the application of the given function to the success value.
     *
     * @param f The function to execute on the success value.
     * @return The failing value or the application of the given function to the success value.
     */
    public E on(final F<T, E> f) {
      return v.toEither().left().on(f);
    }

    /**
     * Executes a side-effect on the failing value if there is one.
     *
     * @param f The side-effect to execute.
     * @return The unit value.
     */
    public Unit foreach(final F<E, Unit> f) {
      return v.toEither().left().foreach(f);
    }

    /**
     * Executes a side-effect on the failing value if there is one.
     *
     * @param f The side-effect to execute.
     */
    public void foreach(final Effect<E> f) {
      v.toEither().left().foreach(f);
    }

    /**
     * Maps the given function across the failing side of this validation.
     *
     * @param f The function to map.
     * @return A new validation with the function mapped.
     */
    public <A> Validation<A, T> map(final F<E, A> f) {
      return Validation.validation(v.toEither().left().map(f));
    }

    /**
     * Binds the given function across this validation's failing value if it has one.
     *
     * @param f The function to bind across this validation.
     * @return A new validation value after binding.
     */
    public <A> Validation<A, T> bind(final F<E, Validation<A, T>> f) {
      return v.isFail() ? f.f(v.fail()) : Validation.<A, T>success(v.success());
    }

    /**
     * Performs a bind across the validation, but ignores the element value in the function.
     *
     * @param v The validation value to apply in the final join.
     * @return A new validation value after the final join.
     */
    public <A> Validation<A, T> sequence(final Validation<A, T> v) {
      return bind(new F<E, Validation<A, T>>() {
        public Validation<A, T> f(final E e) {
          return v;
        }
      });
    }

    /**
     * Returns <code>None</code> if this is a success or if the given predicate <code>p</code> does not hold for the
     * failing value, otherwise, returns a fail in <code>Some</code>.
     *
     * @param f The predicate function to test on this failing value.
     * @return <code>None</code> if this is a success or if the given predicate <code>p</code> does not hold for the
     * failing value, otherwise, returns a fail in <code>Some</code>.
     */
    public <A> Option<Validation<E, A>> filter(final F<E, Boolean> f) {
      return v.toEither().left().<A>filter(f).map(Validation.<E, A>validation());
    }

    /**
     * Function application on the failing value.
     *
     * @param v The validation of the function to apply on the failing value.
     * @return The result of function application in validation.
     */
    public <A> Validation<A, T> apply(final Validation<F<E, A>, T> v) {
      return v.f().bind(new F<F<E, A>, Validation<A, T>>() {
        public Validation<A, T> f(final F<E, A> f) {
          return map(f);
        }
      });
    }

    /**
     * Returns <code>true</code> if this is a success or returns the result of the application of the given
     * function to the failing value.
     *
     * @param f The predicate function to test on this failing value.
     * @return <code>true</code> if this is a success or returns the result of the application of the given
     * function to the failing value.
     */
    public boolean forall(final F<E, Boolean> f) {
      return v.toEither().left().forall(f);
    }

    /**
     * Returns <code>false</code> if this is a success or returns the result of the application of the given
     * function to the failing value.
     *
     * @param f The predicate function to test on this failing value.
     * @return <code>false</code> if this is a success or returns the result of the application of the given
     * function to the failing value.
     */
    public boolean exists(final F<E, Boolean> f) {
      return v.toEither().left().exists(f);
    }

    /**
     * Returns a single element list if this is a failing value, otherwise an empty list.
     *
     * @return A single element list if this is a failing value, otherwise an empty list.
     */
    public List<E> toList() {
      return v.toEither().left().toList();
    }

    /**
     * Returns the failing value in <code>Some</code> if there is one, otherwise <code>None</code>.
     *
     * @return The failing value in <code>Some</code> if there is one, otherwise <code>None</code>.
     */
    public Option<E> toOption() {
      return v.toEither().left().toOption();
    }

    /**
     * Returns a single element array if this is a failing value, otherwise an empty list.
     *
     * @return A single element array if this is a failing value, otherwise an empty list.
     */
    public Array<E> toArray() {
      return v.toEither().left().toArray();
    }

    /**
     * Returns a single element array if this is a failing value, otherwise an empty list.
     *
     * @param c The class type of the array to return.
     * @return A single element array if this is a failing value, otherwise an empty list.
     */
    public Array<E> toArray(final Class<E[]> c) {
      return v.toEither().left().toArray(c);
    }

    /**
     * Returns a single element stream if this is a failing value, otherwise an empty list.
     *
     * @return A single element stream if this is a failing value, otherwise an empty list.
     */
    public Stream<E> toStream() {
      return v.toEither().left().toStream();
    }

    /**
     * Returns an iterator for this projection. This method exists to permit the use in a <code>for</code>-each loop.
     *
     * @return A iterator for this projection.
     */
    public Iterator<E> iterator() {
      return v.toEither().left().iterator();
    }
  }

  /**
   * Puts this validation's failing value in a non-empty list if there is one.
   *
   * @return A validation with its failing value in a non-empty list if there is one. 
   */
  @SuppressWarnings({"unchecked"})
  public Validation<NonEmptyList<E>, T> nel() {
    return isSuccess() ?
        Validation.<NonEmptyList<E>, T>success(success()) :
        Validation.<NonEmptyList<E>, T>fail(NonEmptyList.nel(fail()));
  }

  /**
   * Construct a validation using the given either value.
   *
   * @param e The either value to construct a validation with.
   * @return A validation using the given either value.
   */
  public static <E, T> Validation<E, T> validation(final Either<E, T> e) {
    return new Validation<E, T>(e);
  }

  /**
   * Returns a function that constructs a validation with an either.
   *
   * @return A function that constructs a validation with an either.
   */
  public static <E, T> F<Either<E, T>, Validation<E, T>> validation() {
    return new F<Either<E, T>, Validation<E, T>>() {
      public Validation<E, T> f(final Either<E, T> e) {
        return validation(e);
      }
    };
  }

  /**
   * Returns a function that constructs an either with a validation.
   *
   * @return A function that constructs an either with a validation.
   */
  public static <E, T> F<Validation<E, T>, Either<E, T>> either() {
    return new F<Validation<E, T>, Either<E, T>>() {
      public Either<E, T> f(final Validation<E, T> v) {
        return v.toEither();
      }
    };
  }

  /**
   * Returns a succeeding validation containing the given value.
   *
   * @param t The value to use in the succeeding validation.
   * @return A succeeding validation containing the given value.
   */
  public static <E, T> Validation<E, T> success(final T t) {
    return validation(Either.<E, T>right(t));
  }

  /**
   * Returns a failing validation containing the given value.
   *
   * @param e The value to use in the failing validation.
   * @return A failing validation containing the given value.
   */
  public static <E, T> Validation<E, T> fail(final E e) {
    return validation(Either.<E, T>left(e));
  }

  /**
   * Returns a failing validation containing a non-empty list that contains the given value.
   *
   * @param e The value to use in a non-empty list for the failing validation.
   * @return A failing validation containing a non-empty list that contains the given value.
   */
  public static <E, T> Validation<NonEmptyList<E>, T> failNEL(final E e) {
    return fail(NonEmptyList.nel(e));
  }

  /**
   * Returns a validation based on a boolean condition. If the condition is <code>true</code>, the validation succeeds,
   * otherwise it fails.
   *
   * @param c The condition to base the returned validation on.
   * @param e The failing value to use if the condition is <code>false</code>.
   * @param t The succeeding value to use if the condition is <code>true</code>.
   * @return A validation based on a boolean condition.
   */
  public static <E, T> Validation<E, T> condition(final boolean c, final E e, final T t) {
    return c ? Validation.<E, T>success(t) : Validation.<E, T>fail(e);
  }

  /**
   * Parses the given string into a byte.
   *
   * @param s The string to parse.
   * @return A successfully parse byte or a failing exception.
   */
  public static Validation<NumberFormatException, Byte> parseByte(final String s) {
    try {
      return success(Byte.parseByte(s));
    } catch(NumberFormatException e) {
      return fail(e);
    }
  }

  /**
   * Parses the given string into a double.
   *
   * @param s The string to parse.
   * @return A successfully parse double or a failing exception.
   */
  public static Validation<NumberFormatException, Double> parseDouble(final String s) {
    try {
      return success(Double.parseDouble(s));
    } catch(NumberFormatException e) {
      return fail(e);
    }
  }

  /**
   * Parses the given string into a float.
   *
   * @param s The string to parse.
   * @return A successfully parse float or a failing exception.
   */
  public static Validation<NumberFormatException, Float> parseFloat(final String s) {
    try {
      return success(Float.parseFloat(s));
    } catch(NumberFormatException e) {
      return fail(e);
    }
  }

  /**
   * Parses the given string into a integer.
   *
   * @param s The string to parse.
   * @return A successfully parse integer or a failing exception.
   */
  public static Validation<NumberFormatException, Integer> parseInt(final String s) {
    try {
      return success(Integer.parseInt(s));
    } catch(NumberFormatException e) {
      return fail(e);
    }
  }

  /**
   * Parses the given string into a long.
   *
   * @param s The string to parse.
   * @return A successfully parse long or a failing exception.
   */
  public static Validation<NumberFormatException, Long> parseLong(final String s) {
    try {
      return success(Long.parseLong(s));
    } catch(NumberFormatException e) {
      return fail(e);
    }
  }

  /**
   * Parses the given string into a short.
   *
   * @param s The string to parse.
   * @return A successfully parse short or a failing exception.
   */
  public static Validation<NumberFormatException, Short> parseShort(final String s) {
    try {
      return success(Short.parseShort(s));
    } catch(NumberFormatException e) {
      return fail(e);
    }
  }
}
