package scalaz

import Scalaz._

/** The input to an iteratee. **/
sealed trait Input[E] {
  def apply[Z](empty: => Z, el: (=> E) => Z, eof: => Z): Z
}

/** A pure iteratee computation which is either done or needs more input **/
sealed trait Iteratee[E, A] {
  import Iteratee._
  def fold[Z](done: (=> A, => Input[E]) => Z, cont: (Input[E] => Iteratee[E, A]) => Z): Z
  def apply[F[_]](f: F[E])(implicit e: Enumerator[F]): Iteratee[E, A] = e(f, this)
  def run: Option[A] = {
    def runCont(i: Iteratee[E, A]) = i.fold(done = (x, _) => Some(x), cont = _ => None)
    fold(done = (x, _) => Some(x),
          cont = k => runCont(k(EOF[E])))
  }
}

/** An Enumerator[F] feeds data from an F to an iteratee **/
trait Enumerator[F[_]] {
  def apply[E, A](f: F[E], i: Iteratee[E, A]): Iteratee[E, A]
}

object Iteratee {

  /** A computation that has finished **/
  object Done {
    def apply[E, A](a: => A, i: => Input[E]) = new Iteratee[E, A] {
      def fold[Z](done: (=> A, => Input[E]) => Z,
                  cont: (Input[E] => Iteratee[E, A]) => Z): Z = done(a, i)
    }
    def unapply[E, A](r: Iteratee[E, A]) =
      r.fold[Either[Iteratee[E, A], (A,Input[E])]](
        done = (a, i) => Right((a, i)),
        cont = f => Left(Cont(f))).left.toOption
  }

  /** A computation that takes an element from an input to yield a new computation **/
  object Cont {
    def apply[E, A](f: Input[E] => Iteratee[E, A]) = new Iteratee[E, A] {
      def fold[Z](done: (=> A, => Input[E]) => Z,
                  cont: (Input[E] => Iteratee[E, A]) => Z): Z = cont(f)
    }
    def unapply[E, A](r: Iteratee[E, A]) =
      r.fold[Either[Iteratee[E, A], Input[E] => Iteratee[E, A]]](
        done = (a, i) => Left(Done(a, i)),
        cont = f => Right(f)).left.toOption
  }

  /** An iteratee that consumes the head of the input **/
  def head[E] = {
    def step(s: Input[E]): Iteratee[E, Option[E]] =
      s(el = e => Done(Some(e), Empty[E]),
        empty = Cont(step),
        eof = Done(None, EOF[E]))
    Cont(step)
  }

  /** An iteratee that returns the first element of the input **/
  def peek[E] = {
    def step(s: Input[E]): Iteratee[E, Option[E]]
      = s(el = e => Done(Some(e), s),
          empty = Cont(step),
          eof = Done(None, EOF[E]))
    Cont(step)
  }

  /** An iteratee that skips the first n elements of the input **/
  def drop[E](n: Int): Iteratee[E, Unit] = {
    def step(s: Input[E]): Iteratee[E, Unit] =
      s(el = _ => drop(n - 1),
        empty = Cont(step),
        eof = Done((), EOF[E]))
    if (n == 0) Done((), Empty[E])
    else Cont(step)
  }

  /** An iteratee that counts and consumes the elements of the input **/
  def length[E] = {
    def step(acc: Int)(s: Input[E]): Iteratee[E, Int] =
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
    def unapply[E](r: Input[E]) =
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
    def unapply[E](r: Input[E]) =
      r.apply[Either[Input[E], (E)]](
        empty = Left(Empty[E]),
        el = e => Right(e),
        eof = Left(EOF[E])).left.toOption
  }

  /** Input that is exhausted **/
  object EOF {
    def apply[E] = new Input[E] {
      def apply[Z](empty: => Z, el: (=> E) => Z, eof: => Z): Z = eof
    }
    def unapply[E](r: Input[E]) =
      r.apply[Either[Input[E], Boolean]](
        empty = Left(Empty[E]),
        el = e => Left(El(e)),
        eof = Right(true)).fold(x => false, x => x)
  }
}

