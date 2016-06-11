package scalaz
package iteratee

import Iteratee._
import Id._

trait IterateeFunctions {
  def iteratee[E, A](s: Step[E, A]): Iteratee[E, A] =
    iterateeT[E, Id, A](s)

  /**
   * Repeats the given iteratee by appending with the given monoid.
   */
  def repeatBuild[E, A, F[_]](iter: Iteratee[E, A])(implicit mon: Monoid[F[A]], F: Applicative[F]): Iteratee[E, F[A]] = {
    import Iteratee._
    def step(acc: F[A])(s: Input[E]): Iteratee[E, F[A]] =
      s(el = e => iter.foldT[Iteratee[E, F[A]]](
        done = (a, _) => cont(step(mon.append(acc, F.point(a)))),
        cont = k => k(elInput(e)).foldT(
          done = (a, _) => cont(step(mon.append(acc, F.point(a)))),
          cont = k2 => cont((in: Input[E]) => for {
            h <- k2(in)
            t <- this.repeatBuild[E, A, F](iter)
          } yield mon.append(acc, mon.append(F.point(h), t)))
        )),
        empty = cont(step(acc)),
        eof = done(acc, eofInput))
    cont(step(mon.zero))
  }

  /**
   * Iteratee that collects all inputs with the given monoid.
   */
  def collect[A, F[_]](implicit mon: Monoid[F[A]], pt: Applicative[F]): Iteratee[A, F[A]] = {
    fold[A, Id, F[A]](mon.zero)((acc, e) => mon.append(acc, pt.point(e)))
  }

  /**
   * Iteratee that collects all inputs in reverse with the given reducer.
   *
   * This iteratee is useful for F[_] with efficient cons, i.e. List.
   */
  def reversed[A, F[_]](implicit r: Reducer[A, F[A]]): Iteratee[A, F[A]] = {
    fold[A, Id, F[A]](r.monoid.zero)((acc, e) => r.cons(e, acc))
  }

  /**
   * Iteratee that collects the first n inputs.
   */
  def take[A, F[_]](n: Int)(implicit mon: Monoid[F[A]], pt: Applicative[F]): Iteratee[A, F[A]] = {
    def loop(acc: F[A], n: Int)(s: Input[A]): Iteratee[A, F[A]] =
      s(el = e =>
        if (n <= 0) done[A, Id, F[A]](acc, s)
        else cont(loop(mon.append(acc, pt.point(e)), n - 1))
        , empty = cont(loop(acc, n))
        , eof = done[A, Id, F[A]](acc, s)
      )
    cont(loop(mon.zero, n))
  }

  /**
   * Iteratee that collects inputs with the given monoid until the input element fails a test.
   */
  def takeWhile[A, F[_]](p: A => Boolean)(implicit mon: Monoid[F[A]], pt: Applicative[F]): Iteratee[A, F[A]] = {
    def loop(acc: F[A])(s: Input[A]): Iteratee[A, F[A]] =
      s(el = e =>
        if (p(e)) cont(loop(mon.append(acc, pt.point(e))))
        else done[A, Id, F[A]](acc, s)
        , empty = cont(loop(acc))
        , eof = done[A, Id, F[A]](acc, eofInput)
      )
    cont(loop(mon.zero))
  }

  /**
   * Iteratee that collects inputs with the given monoid until the input element passes a test.
   */
  def takeUntil[A, F[_]](p: A => Boolean)(implicit mon: Monoid[F[A]], pt: Applicative[F]): Iteratee[A, F[A]] =
    takeWhile(!p(_))

  /**
   * Produces chunked output split by the given predicate.
   */
  def groupBy[A, F[_]](pred: (A, A) => Boolean)(implicit mon: Monoid[F[A]], pr: Applicative[F]): Iteratee[A, F[A]] = {
    Iteratee.peek[A, Id] flatMap {
      case None => done(Monoid[F[A]].zero, Input.Empty[A])
      case Some(h) => takeWhile(pred(_, h))
    }
  }

}
