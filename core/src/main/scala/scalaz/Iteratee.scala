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
  def run: Option[A] = {
    def runCont(i: IterV[E, A]) = i.fold(done = (x, _) => Some(x), cont = _ => None)
    fold(done = (x, _) => Some(x),
          cont = k => runCont(k(EOF[E])))
  }
}

/** Monadic Iteratees **/
sealed trait IterVM[M[_], E, A] {
  import IterV._
  def fold[Z](done: (=> A, => Input[E]) => Z, cont: (Input[E] => Iteratee[M, E, A]) => Z)
}

case class Iteratee[M[_], E, A](value: M[IterVM[M, E, A]]) extends NewType[M[IterVM[M, E, A]]]

/** An Enumerator[F] feeds data from an F to an iteratee **/
trait Enumerator[F[_]] {
  def apply[E, A](f: F[E], i: IterV[E, A]): IterV[E, A]
}


object IterV {
  /** An EnumeratorM[M, _, _] feeds data in a monad M to an iteratee **/
  type EnumeratorM[M[_], E, A] = IterV[E, A] => M[IterV[E, A]]

  /** A computation that has finished **/
  object Done {
    def apply[E, A](a: => A, i: => Input[E]) = new IterV[E, A] {
      def fold[Z](done: (=> A, => Input[E]) => Z,
                  cont: (Input[E] => IterV[E, A]) => Z): Z = done(a, i)
    }
    def unapply[E, A](r: IterV[E, A]): Option[(A, Input[E])] =
      r.fold[Either[IterV[E, A], (A,Input[E])]](
        done = (a, i) => Right((a, i)),
        cont = f => Left(Cont(f))).right.toOption
  }

  /** A computation that takes an element from an input to yield a new computation **/
  object Cont {
    def apply[E, A](f: Input[E] => IterV[E, A]) = new IterV[E, A] {
      def fold[Z](done: (=> A, => Input[E]) => Z,
                  cont: (Input[E] => IterV[E, A]) => Z): Z = cont(f)
    }
    def unapply[E, A](r: IterV[E, A]): Option[Input[E] => IterV[E, A]] =
      r.fold[Either[IterV[E, A], Input[E] => IterV[E, A]]](
        done = (a, i) => Left(Done(a, i)),
        cont = f => Right(f)).right.toOption
  }

  /** An iteratee that consumes the head of the input **/
  def head[E] = {
    def step(s: Input[E]): IterV[E, Option[E]] =
      s(el = e => Done(Some(e), Empty[E]),
        empty = Cont(step),
        eof = Done(None, EOF[E]))
    Cont(step)
  }

  /** An iteratee that returns the first element of the input **/
  def peek[E] = {
    def step(s: Input[E]): IterV[E, Option[E]]
      = s(el = e => Done(Some(e), s),
          empty = Cont(step),
          eof = Done(None, EOF[E]))
    Cont(step)
  }

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
  def length[E] = {
    def step(acc: Int)(s: Input[E]): IterV[E, Int] =
      s(el = _ => Cont(step(acc + 1)),
        empty = Cont(step(acc)),
        eof = Done(acc, EOF[E]))
    Cont(step(0))
  }

  /** Input that has a value available **/
  object Empty {
    def apply[E] = new Input[E] {
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
    def apply[E](e0: => E) = new Input[E] {
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
    def apply[E] = new Input[E] {
      def apply[Z](empty: => Z, el: (=> E) => Z, eof: => Z): Z = eof
    }
    def unapply[E](r: Input[E]): Boolean =
      r.apply[Either[Input[E], Boolean]](
        empty = Left(Empty[E]),
        el = e => Left(El(e)),
        eof = Right(true)).fold(x => false, x => x)
  }
}

