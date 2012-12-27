// Legacy implementation of iteratees, for compatibility with Scalaz 6 */
package scalaz
import Scalaz._

/** The input to an iteratee. */
sealed trait Input[E] {
  def apply[Z](empty: => Z, el: (=> E) => Z, eof: => Z): Z
  def map[D](f: E => D): Input[D] =
    apply(IterV.Empty.apply[D], e => IterV.El(f(e)), IterV.EOF.apply[D])
}

/** A pure iteratee computation which is either done or needs more input */
sealed trait IterV[E, A] {
  import IterV._

  def fold[Z](done: (=> A, => Input[E]) => Z, cont: (Input[E] => IterV[E, A]) => Z): Z
  def apply[F[_]](f: F[E])(implicit e: Enumerator[F]): IterV[E, A] = e(f, this)
  def run: A = {
    def runCont(i: IterV[E, A]) = i.fold(done = (x, _) => Some(x), cont = _ => None)
    fold(done = (x, _) => x,
          cont = k => runCont(k(EOF[E])).getOrElse(sys.error("Diverging iteratee!")))
  }
  def drop1First: IterV[E, A] = drop(1) flatMap (_ => this)
  def feed(e: Input[E]): IterV[E, A] = fold((_, _) => this, k => k(e))
  def mapInput(f: Input[E] => Input[E]): IterV[E, A] =
    fold((a, e) => Done(a, f(e)), (k => Cont(i => k(f(i)))))
  def filterInput(p: E => Boolean): IterV[E, A] =
    mapInput {
      case i@El(e) => if (p(e)) i else Empty.apply
      case x => x
    }
  def zipWith[B, C](eb: IterV[E, B], f: (A, B) => C): IterV[E, C] = {
    (this, eb) match {
      case (Cont(k), Cont(l)) => Cont(ie => k(ie).zipWith(l(ie), f))
      case (Cont(k), b) => Cont(ie => k(ie).zipWith(b, f))
      case (a, Cont(k)) => Cont(ie => a.zipWith(k(ie), f))
      case (Done(a, e), Done(b, _)) => Done(f(a, b), e)
    }
  }

  /** An iteratee that sends itself the EOF signal after `n` inputs. */
  def take(n: Int): IterV[E, A] =
    fold((a, i) => Done(a, i),
         k => if (n <= 0) k(EOF.apply) else Cont(i => i(
           k(i).take(n),
           e => k(i).take(n - 1),
           k(i)
         )))

  /** Lift into a monadic iteratee, in the Id monad. */
  def lift: Iteratee[Id, E, A] =
    fold(done = (a, e) => Iteratee[Id, E, A](DoneM(a, e)),
         cont = k => Iteratee[Id, E, A](ContM(i => k(i).lift)))

  def flatMap[B](f: A => IterV[E, B]): IterV[E, B] = this.fold(
      done = (x, str) => f(x).fold(
        done = (x2, _) => Done(x2, str),
        cont = _(str)),
      cont = k => Cont(str2 => k(str2) flatMap f))

  def map[B](f: A => B): IterV[E, B] = flatMap(a => Done(f(a), Empty[E]))
}

/** Monadic Iteratees */
sealed trait IterVM[M[+_], E, A] {
  import IterV._
  def fold[Z](done: (=> A, => Input[E]) => Z, cont: (Input[E] => Iteratee[M, E, A]) => Z): Z
}

case class Iteratee[M[+_], E, A](value: M[IterVM[M, E, A]]) {
  import IterV._

  def apply(es: StreamT[M, E])(implicit M: Monad[M]): M[A] = for {
    i <- value
    p <- es.uncons
    v <- i.fold((a, e) => M.pure(a),
                k => p match {
                  case Some((h, t)) => k(El(h))(t)
                  case None => k(EOF[E]).value.map(
                    _.fold((a, e) => a, _ => sys.error("Diverging Iteratee")))
                })
  } yield v

  def lower(f: M ~> Id)(implicit M: Monad[M]): M[IterV[E, A]] = for {
    v <- value
    a <- M.pure(v match {
      case DoneM(a, e) => Done(a, e)
      case ContM(k) => Cont((e: Input[E]) => f(k(e).lower(f)))
    })
  } yield a

  def xmap[D](f: D => E, g: E => D)(implicit M: Monad[M]): Iteratee[M, D, A] = Iteratee[M, D, A](for {
    v <- value
    h <- M.pure(v.fold((a, e) => DoneM[M, D, A](a, e map g),
                k => ContM[M, D, A]((d: Input[D]) =>
                  k(d map f).xmap(f, g))))
  } yield h)

  def map[B](f: A => B)(implicit M: Functor[M]): Iteratee[M, E, B] =
    Iteratee[M, E, B](M.map(value)((x: IterVM[M, E, A]) => x.fold((a, e) => DoneM[M, E, B](f(a), e), k => ContM[M, E, B](e => k(e).map(f)))))

  def run(implicit M: Monad[M]): M[A] = {
    def runCont(i: Iteratee[M, E, A]) = for {
      v <- i.value
    } yield v.fold(done = (x, _) => Some(x), cont = _ => None)
    for {
      v <- value
      t <- v.fold(done = (x, _) => M.point(Some(x)),
                  cont = k => runCont(k(EOF[E])))
    } yield t.getOrElse(sys.error("Diverging iteratee!"))
  }
}

/** An Enumerator[F] feeds data from an F to an iteratee */
trait Enumerator[F[_]] {
  def apply[E, A](f: F[E], i: IterV[E, A]): IterV[E, A]
}


object IterV {

  case class OptionSyntax[A](value: Option[A]) {
    /**
     * Returns a Done iteratee with the given value if the Option is not defined, otherwise runs the given function.
     */
    def iterDoneOr[B](b: => B, f: A => IterV[A, B]): IterV[A, B] = value match {
      case None => IterV.Done(b, IterV.EOF.apply)
      case Some(a) => f(a)
    }
  }

  implicit def wrapOptionSyntax[A](value: Option[A]): OptionSyntax[A] = OptionSyntax(value)

  implicit def iterVMonad[E]: Monad[({type λ[α] = IterV[E,α]})#λ] = new Monad[({type λ[α] = IterV[E,α]})#λ] {
    def bind[A,B](a: IterV[E, A])(f: A => IterV[E, B]) = a.flatMap(f)
    def point[A](a: => A): IterV[E, A] = Done(a, Empty[E])
  }

  /** An EnumeratorM[M, _, _] feeds data in a monad M to an iteratee */
  type EnumeratorM[M[_], E] = ({type λ[α] = IterV[E, α]})#λ ~> ({type λ[α] = M[IterV[E, α]]})#λ

  /** A computation that has finished */
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

  /** A computation that takes an element from an input to yield a new computation */
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

  /** A monadic computation that has finished */
  object DoneM {
    def apply[M[+_], E, A](a: => A, i: => Input[E]): IterVM[M, E, A] = new IterVM[M, E, A] {
      def fold[Z](done: (=> A, => Input[E]) => Z,
                  cont: (Input[E] => Iteratee[M, E, A]) => Z): Z = done(a, i)
    }
    def unapply[M[+_], E, A](r: IterVM[M, E, A]): Option[(A, Input[E])] =
      r.fold[Option[(A, Input[E])]](
        done = (a, i) => Some((a, i)),
        cont = f => None)
  }

  object ContM {
    def apply[M[+_], E, A](f: Input[E] => Iteratee[M, E, A]): IterVM[M, E, A] = new IterVM[M, E, A] {
      def fold[Z](done: (=> A, => Input[E]) => Z,
                  cont: (Input[E] => Iteratee[M, E, A]) => Z): Z = cont(f)
    }
    def unapply[M[+_], E, A](r: IterVM[M, E, A]): Option[Input[E] => Iteratee[M, E, A]] =
      r.fold[Option[Input[E] => Iteratee[M, E, A]]](
        done = (a, i) => None,
        cont = f => Some(f))
  }

  /** An iteratee that consumes the head of the input */
  def head[E] : IterV[E, Option[E]] = {
    def step(s: Input[E]): IterV[E, Option[E]] =
      s(el = e => Done(Some(e), Empty[E]),
        empty = Cont(step),
        eof = Done(None, EOF[E]))
    Cont(step)
  }

  /** An iteratee that returns the first element of the input */
  def peek[E] : IterV[E, Option[E]] = {
    def step(s: Input[E]): IterV[E, Option[E]]
      = s(el = e => Done(Some(e), s),
          empty = Cont(step),
          eof = Done(None, EOF[E]))
    Cont(step)
  }

  /** Peeks and returns either a Done iteratee with the given value or runs the given function with the peeked value */
  def peekDoneOr[A, B](b: => B, f: A => IterV[A, B]): IterV[A, B] =
    peek[A] flatMap (_.iterDoneOr(b, f))

  /** An iteratee that skips the first n elements of the input */
  def drop[E](n: Int): IterV[E, Unit] = {
    def step(s: Input[E]): IterV[E, Unit] =
      s(el = _ => drop(n - 1),
        empty = Cont(step),
        eof = Done((), EOF[E]))
    if (n == 0) Done((), Empty[E])
    else Cont(step)
  }

  /** An iteratee that counts and consumes the elements of the input */
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
  def takeWhile[A, F[_]](pred: A => Boolean)(implicit mon: Monoid[F[A]], pr: Applicative[F]): IterV[A, F[A]] = {
    def peekStepDoneOr(z: F[A]) = peekDoneOr(z, step(z, _: A))

    def step(acc: F[A], a: A): IterV[A, F[A]] = {
      if (pred(a))
        drop(1).flatMap(_ => peekStepDoneOr(acc |+| pr.point(a)))
      else
        Done(acc, EOF.apply)
    }
    peekStepDoneOr(mzero[F[A]])
  }

  /**
   * Produces chunked output split by the given predicate.
   */
  def groupBy[A, F[_]](pred: (A, A) => Boolean)(implicit mon: Monoid[F[A]], pr: Applicative[F]): IterV[A, F[A]] = {
    IterV.peek flatMap {
      case None => Done(mzero[F[A]], Empty[A])
      case Some(h) => takeWhile(pred(_, h))
    }
  }

  /**
   * Repeats the given iteratee by appending with the given monoid.
   */
  def repeat[E, A, F[_]](iter: IterV[E,A])(implicit mon: Monoid[F[A]], pr: Applicative[F]): IterV[E, F[A]] = {
    def step(acc: F[A])(s: Input[E]): IterV[E, F[A]] =
      s(el = e => iter.fold(
          (a, _) => Cont(step(acc |+| pr.point(a))),
          k => k(El(e)).fold(
            (a, _) => Cont(step(acc |+| pr.point(a))),
            (k2) => Cont((in: Input[E]) => for {
                h <- k2(in)
                t <- repeat(iter)(mon, pr)
              } yield acc |+| pr.point(h) |+| t
            )
          )),
        empty = Cont(step(acc)),
        eof = Done(acc, EOF.apply))
    Cont(step(mzero[F[A]]))
  }

  /**
   * Iteratee that collects all inputs with the given monoid.
   */
  def collect[A, F[_]](implicit r: Reducer[A, F[A]]): IterV[A, F[A]] = {
    import r._
    def step(acc: F[A])(s: Input[A]): IterV[A, F[A]] =
        s(el = e => Cont(step(snoc(acc, e))),
          empty = Cont(step(acc)),
          eof = Done(acc, EOF.apply))
    Cont(step(mzero[F[A]]))
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

  /**
   * Iteratee that sums the inputs with a given reducer.
   */
  def sum[A, B](implicit r: Reducer[A, B]): IterV[A, B] = {
    def step(acc: B)(s: Input[A]): IterV[A, B] =
        s(el = e => Cont(step(r.snoc(acc, e))),
          empty = Cont(step(acc)),
          eof = Done(acc, EOF.apply))
    Cont(step(r.monoid.zero))
  }

  /** Input that has no values available */
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

  /** Input that has a value available  */
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

  /** Input that is exhausted */
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
