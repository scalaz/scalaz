package scalaz

import Scalaz._

/** The input to an iteratee. **/
sealed trait Input[E] {
  def apply[Z](empty: => Z, el: (=> E) => Z, eof: => Z): Z
}

/** A pure iteratee computation which is either done or needs more input **/
sealed trait IterV[E, A] {
  import IterV._
  def fold[Z](done: (=> A, => Input[E]) => Z, cont: (Input[E] => IterV[E, A]) => Z): Z
  def apply[F[_]](f: F[E])(implicit e: Enumerator[F]): IterV[E, A] = e(f, this)
  def run: A = {
    def runCont(i: IterV[E, A]) = i.fold(done = (x, _) => Some(x), cont = _ => None)
    fold(done = (x, _) => x,
          cont = k => runCont(k(EOF[E])).getOrElse(error("Diverging iteratee!")))
  }
  def drop1First: IterV[E, A] = drop(1) flatMap (_ => this)
}

/** Monadic Iteratees **/
sealed trait IterVM[M[_], E, A] {
  import IterV._
  def fold[Z](done: (=> A, => Input[E]) => Z, cont: (Input[E] => Iteratee[M, E, A]) => Z): Z
}

case class Iteratee[M[_], E, A](value: M[IterVM[M, E, A]]) extends NewType[M[IterVM[M, E, A]]]

/** An Enumerator[F] feeds data from an F to an iteratee **/
trait Enumerator[F[_]] {
  def apply[E, A](f: F[E], i: IterV[E, A]): IterV[E, A]
}


object IterV {
  /** An EnumeratorM[M, _, _] feeds data in a monad M to an iteratee **/
  type EnumeratorM[M[_], E] = ({type λ[α] = IterV[E, α]})#λ ~> ({type λ[α] = M[IterV[E, α]]})#λ

  /** A computation that has finished **/
  object Done {
    def apply[E, A](a: => A, i: => Input[E]): IterV[E, A] = new IterV[E, A] {
      def fold[Z](done: (=> A, => Input[E]) => Z,
                  cont: (Input[E] => IterV[E, A]) => Z): Z = done(a, i)
    }
    def unapply[E, A](r: IterV[E, A]): Option[(A, Input[E])] =
      r.fold[Option[(A,Input[E])]](
        done = (a, i) => Some((a, i)),
        cont = f => None)
  }

  /** A computation that takes an element from an input to yield a new computation **/
  object Cont {
    def apply[E, A](f: Input[E] => IterV[E, A]): IterV[E, A] = new IterV[E, A] {
      def fold[Z](done: (=> A, => Input[E]) => Z,
                  cont: (Input[E] => IterV[E, A]) => Z): Z = cont(f)
    }
    def unapply[E, A](r: IterV[E, A]): Option[Input[E] => IterV[E, A]] =
      r.fold[Option[Input[E] => IterV[E, A]]](
        done = (a, i) => None,
        cont = Some(_))
  }

  /** A monadic computation that has finished **/
  object DoneM {
    def apply[M[_], E, A](a: => A, i: => Input[E]): IterVM[M, E, A] = new IterVM[M, E, A] {
      def fold[Z](done: (=> A, => Input[E]) => Z,
                  cont: (Input[E] => Iteratee[M, E, A]) => Z): Z = done(a, i)
    }
    def unapply[M[_], E, A](r: IterVM[M, E, A]): Option[(A, Input[E])] =
      r.fold[Option[(A, Input[E])]](
        done = (a, i) => Some((a, i)),
        cont = f => None)
  }

  object ContM {
    def apply[M[_], E, A](f: Input[E] => Iteratee[M, E, A]): IterVM[M, E, A] = new IterVM[M, E, A] {
      def fold[Z](done: (=> A, => Input[E]) => Z,
                  cont: (Input[E] => Iteratee[M, E, A]) => Z): Z = cont(f)
    }
    def unapply[M[_], E, A](r: IterVM[M, E, A]): Option[Input[E] => Iteratee[M, E, A]] =
      r.fold[Option[Input[E] => Iteratee[M, E, A]]](
        done = (a, i) => None,
        cont = f => Some(f))
  }

  /** An iteratee that consumes the head of the input **/
  def head[E] : IterV[E, Option[E]] = {
    def step(s: Input[E]): IterV[E, Option[E]] =
      s(el = e => Done(Some(e), Empty[E]),
        empty = Cont(step),
        eof = Done(None, EOF[E]))
    Cont(step)
  }

  /** An iteratee that returns the first element of the input **/
  def peek[E] : IterV[E, Option[E]] = {
    def step(s: Input[E]): IterV[E, Option[E]]
      = s(el = e => Done(Some(e), s),
          empty = Cont(step),
          eof = Done(None, EOF[E]))
    Cont(step)
  }

  /** Peeks and returns either a Done iteratee with the given value or runs the given function with the peeked value **/
  def peekDoneOr[A, B](b: => B, f: A => IterV[A, B]): IterV[A, B] =
    peek[A] >>= (_.iterDoneOr(b, f))

  /** An iteratee that skips the first n elements of the input **/
  def drop[E](n: Int): IterV[E, Unit] = {
    def step(s: Input[E]): IterV[E, Unit] =
      s(el = _ => drop(n - 1),
        empty = Cont(step),
        eof = Done((), EOF[E]))
    if (n == 0) Done((), Empty[E])
    else Cont(step)
  }

  /** An iteratee that counts and consumes the elements of the input **/
  def length[E] : IterV[E, Int] = {
    def step(acc: Int)(s: Input[E]): IterV[E, Int] =
      s(el = _ => Cont(step(acc + 1)),
        empty = Cont(step(acc)),
        eof = Done(acc, EOF[E]))
    Cont(step(0))
  }

  /**
   * Takes while the given predicate holds, appending with the given monoid.
   */
  def takeWhile[A, F[_]](pred: A => Boolean)(implicit mon: Monoid[F[A]], pr: Pure[F]): IterV[A, F[A]] = {
    def peekStepDoneOr(z: F[A]) = peekDoneOr(z, step(z, _: A))

    def step(acc: F[A], a: A): IterV[A, F[A]] = {
      if (pred(a))
        drop(1) >>=| peekStepDoneOr(acc |+| a.η[F])
      else
        Done(acc, EOF.apply)
    }
    peekStepDoneOr(∅[F[A]])
  }

  /**
   * Produces chunked output split by the given predicate.
   */
  def groupBy[A, F[_]](pred: (A, A) => Boolean)(implicit mon: Monoid[F[A]], pr: Pure[F]): IterV[A, F[A]] = {
    IterV.peek >>= {
      case None => Done(∅[F[A]], Empty[A])
      case Some(h) => takeWhile(pred(_, h))
    }
  }

  /**
   * Repeats the given iteratee by appending with the given monoid.
   */
  def repeat[E, A, F[_]](iter: IterV[E,A])(implicit mon: Monoid[F[A]], pr: Pure[F]): IterV[E, F[A]] = {
    def step(acc: F[A])(s: Input[E]): IterV[E, F[A]] =
      s(el = e => iter.fold(
          (a, _) => Cont(step(acc |+| a.η[F])),
          k => k(El(e)).fold(
            (a, _) => Cont(step(acc |+| a.η[F])),
            (k2) => Cont(step(acc))
          )),
        empty = Cont(step(acc)),
        eof = Done(acc, EOF.apply))
    Cont(step(∅[F[A]]))
  }

  /**
   * Iteratee that collects all inputs with the given monoid.
   */
  def collect[A, F[_]](implicit mon: Monoid[F[A]], pr: Pure[F]): IterV[A, F[A]] = {
    def step(acc: F[A])(s: Input[A]): IterV[A, F[A]] =
        s(el = e => Cont(step(acc |+| e.η[F])),
          empty = Cont(step(acc)),
          eof = Done(acc, EOF.apply))
    Cont(step(∅[F[A]]))
  }

  /**
   * Iteratee that collects all inputs in reverse with the given reducer.
   *
   * This iteratee is useful for F[_] with efficient cons, i.e. List.
   */
  def reversed[A, F[_]](implicit r: Reducer[A, F[A]]): IterV[A, F[A]] = {
    def step(acc: F[A])(s: Input[A]): IterV[A, F[A]] =
        s(el = e => Cont(step(r.cons(e, acc))),
          empty = Cont(step(acc)),
          eof = Done(acc, EOF.apply))
    Cont(step(r.monoid.zero))
  }

  /** Input that has a value available **/
  object Empty {
    def apply[E] : Input[E] = new Input[E] {
      def apply[Z](empty: => Z, el: (=> E) => Z, eof: => Z): Z = empty
    }
    def unapply[E](r: Input[E]): Boolean =
        r.apply[Either[Input[E], Boolean]](
          empty = Right(true),
          el = e => Left(El(e)),
          eof = Left(EOF[E])).fold(x => false, x => x)
  }

  /** Input that has no values available  **/
  object El {
    def apply[E](e0: => E): Input[E] = new Input[E] {
      def apply[Z](empty: => Z, el: (=> E) => Z, eof: => Z): Z = el(e0)
    }
    def unapply[E](r: Input[E]): Option[E] =
      r.apply[Either[Input[E], (E)]](
        empty = Left(Empty[E]),
        el = e => Right(e),
        eof = Left(EOF[E])).right.toOption
  }

  /** Input that is exhausted **/
  object EOF {
    def apply[E] : Input[E] = new Input[E] {
      def apply[Z](empty: => Z, el: (=> E) => Z, eof: => Z): Z = eof
    }
    def unapply[E](r: Input[E]): Boolean =
      r.apply[Either[Input[E], Boolean]](
        empty = Left(Empty[E]),
        el = e => Left(El(e)),
        eof = Right(true)).fold(x => false, x => x)
  }
}

