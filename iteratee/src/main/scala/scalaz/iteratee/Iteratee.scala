package scalaz
package iteratee

import Iteratee._

trait IterateeFunctions {
  def iteratee[X, E, A](s: Step[X, E, A]): Iteratee[X, E, A] =
    iterateeT[X, E, Id, A](s)
  
  /**
   * Repeats the given iteratee by appending with the given monoid.
   */
  def repeatBuild[X, E, A, F[_]](iter: Iteratee[X, E, A])(implicit mon: Monoid[F[A]], F: Pointed[F]): Iteratee[X, E, F[A]] = {
    import Id.id

    def step(acc: F[A])(s: Input[E]): Iteratee[X, E, F[A]] =
      s(el = e => iter.foldT[Iteratee[X, E, F[A]]](
        done = (a, _) => cont(step(mon.append(acc, F.point(a)))),
        cont = k => k(elInput(e)).foldT(
          done = (a, _) => cont(step(mon.append(acc, F.point(a)))),
          cont = (k2) => cont(step(acc)),
          err = e => err(e)
        ),
        err = e => err(e)),
        empty = cont(step(acc)),
        eof = done(acc, eofInput))
    cont(step(mon.zero))
  }

  /**
   * Iteratee that collects all inputs with the given monoid.
   */
  def collect[X, A, F[_]](implicit mon: Monoid[F[A]], pt: Pointed[F]): Iteratee[X, A, F[A]] = {
    import Id.id
    fold[X, A, Id, F[A]](mon.zero)((acc, e) => mon.append(acc, pt.point(e)))
  }

  /**
   * Iteratee that collects all inputs in reverse with the given reducer.
   *
   * This iteratee is useful for F[_] with efficient cons, i.e. List.
   */
  def reversed[X, A, F[_]](implicit r: Reducer[A, F[A]]): Iteratee[X, A, F[A]] = {
    import Id.id
    fold[X, A, Id, F[A]](r.monoid.zero)((acc, e) => r.cons(e, acc))
  }

  /**
   * Iteratee that collects the first n inputs.
   */
  def take[X, A, F[_]](n: Int)(implicit mon: Monoid[F[A]], pt: Pointed[F]): Iteratee[X, A, F[A]] = {
    import Id.id
    def loop(acc: F[A], n: Int)(s: Input[A]): Iteratee[X, A, F[A]] =
      s(el = e =>
        if (n <= 0) done[X, A, Id, F[A]](acc, s)
        else cont(loop(mon.append(acc, pt.point(e)), n - 1))
        , empty = cont(loop(acc, n))
        , eof = done[X, A, Id, F[A]](acc, s)
      )
    cont(loop(mon.zero, n))
  }

  /**
   * Iteratee that collects inputs with the given monoid until the input element fails a test.
   */
  def takeWhile[X, A, F[_]](p: A => Boolean)(implicit mon: Monoid[F[A]], pt: Pointed[F]): Iteratee[X, A, F[A]] = {
    import Id.id
    def loop(acc: F[A])(s: Input[A]): Iteratee[X, A, F[A]] =
      s(el = e =>
        if (p(e)) cont(loop(mon.append(acc, pt.point(e))))
        else done[X, A, Id, F[A]](acc, s)
        , empty = cont(loop(acc))
        , eof = done[X, A, Id, F[A]](acc, eofInput)
      )
    cont(loop(mon.zero))
  }

  /**
   * Iteratee that collects inputs with the given monoid until the input element passes a test.
   */
  def takeUntil[X, A, F[_]](p: A => Boolean)(implicit mon: Monoid[F[A]], pt: Pointed[F]): Iteratee[X, A, F[A]] =
    takeWhile(!p(_))
}

object Iteratee
  extends IterateeFunctions
  with IterateeTFunctions
  with EnumeratorTFunctions
  with EnumerateeTFunctions
  with StepTFunctions
  with InputFunctions {

  def apply[X, E, A](s: Step[X, E, A]): Iteratee[X, E, A] = iteratee(s)
}
