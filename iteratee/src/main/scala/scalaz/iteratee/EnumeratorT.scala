package scalaz
package iteratee

import effect._

import Iteratee._

trait EnumeratorTInstances0 {
  implicit def enumeratorTSemigroup[X, E, F[_], A](implicit F0: Bind[F]): Semigroup[EnumeratorT[X, E, F, A]] = new EnumeratorTSemigroup[X, E, F, A] {
    implicit def F = F0
  }
  implicit def enumeratorTPlus[X, E, F[_]](implicit F0: Bind[F]): Plus[({type λ[α]=EnumeratorT[X, E, F, α]})#λ] = new EnumeratorTPlus[X, E, F] {
    implicit def F = F0
  }
}

trait EnumeratorTInstances extends EnumeratorTInstances0 {
  implicit def enumeratorTMonoid[X, E, F[_], A](implicit F0: Monad[F]): Monoid[EnumeratorT[X, E, F, A]] = new EnumeratorTMonoid[X, E, F, A] {
    implicit def F = F0
  }
  implicit def enumeratorTPlusEmpty[X, E, F[_]](implicit F0: Monad[F]): PlusEmpty[({type λ[α]=EnumeratorT[X, E, F, α]})#λ] = new EnumeratorTPlusEmpty[X, E, F] {
    implicit def F = F0
  }
  implicit def enumeratorMonoid[X, E, A]: Monoid[Enumerator[X, E, A]] = new Monoid[Enumerator[X, E, A]] {
    def append(f1: (Step[X, E, A]) => Step[X, E, A], f2: => (Step[X, E, A]) => Step[X, E, A]): Step[X, E, A] => Step[X, E, A] = f1 andThen f2
    def zero: (Step[X, E, A]) => Step[X, E, A] = x => x
  }
}

trait EnumeratorTFunctions {
  def enumEofT[X, E, F[_] : Monad, A](e: (=> X) => IterateeT[X, E, F, A]): EnumeratorT[X, E, F, A] =
    j => {
      j.fold(
        cont = k =>
          k(eofInput) >>== (s =>
            s >-(
              sys.error("diverging iteratee")
              , enumEofT(e) apply s
              , enumEofT(e) apply s
              ))
        , done = (a, _) =>
          StepT.sdone[X, E, F, A](a, eofInput).pointI
        , err = e(_)
      )
    }

  def enumerate[A, O](as: Stream[A]): Enumerator[Unit, A, O] =
    i =>
      as match {
        case Stream.Empty => i
        case x #:: xs     =>
          i.fold(done = (_, _) => i, cont = k => enumerate(xs)(k(elInput(x)).value), err = e => err[Unit, A, Id, O](e).value)
      }

  implicit def enumStream[X, E, F[_] : Monad, A](xs: Stream[E]): EnumeratorT[X, E, F, A] = {
    s =>
      xs match {
        case h #:: t => s.mapCont(_(elInput(h)) >>== enumStream(t))
        case _       => s.pointI
      }
  }

  implicit def enumIterator[X, E, A](x: Iterator[E]): EnumeratorT[X, E, IO, A] = {
    def loop: EnumeratorT[X, E, IO, A] = {
      s =>
        s.mapCont(
          k =>
            if (x.hasNext) {
              val n = x.next()
              k(elInput(n)) >>== loop
            } else s.pointI
        )
    }
    loop
  }

  import java.io._

  implicit def enumReader[X, A](r: Reader): EnumeratorT[X, IoExceptionOr[Char], IO, A] = {
    def loop: EnumeratorT[X, IoExceptionOr[Char], IO, A] = {
      s =>
        s.mapCont(
          k => {
            val i = IoExceptionOr(r.read)
            if (i exists (_ != -1)) k(elInput(i.map(_.toChar))) >>== loop
            else s.pointI
          }
        )
    }
    loop
  }

  def checkCont0[X, E, F[_], A](z: EnumeratorT[X, E, F, A] => (Input[E] => IterateeT[X, E, F, A]) => IterateeT[X, E, F, A])(implicit p: Pointed[F]): EnumeratorT[X, E, F, A] = {
    def step: EnumeratorT[X, E, F, A] = {
      s =>
        s.mapCont(
          k => z(step)(k)
        )
    }
    step
  }

  def checkCont1[S, X, E, F[_], A](z: (S => EnumeratorT[X, E, F, A]) => S => (Input[E] => IterateeT[X, E, F, A]) => IterateeT[X, E, F, A], t: S)(implicit p: Pointed[F]): EnumeratorT[X, E, F, A] = {
    def step: S => EnumeratorT[X, E, F, A] = {
      o => s =>
        s.mapCont(
          k => z(step)(o)(k)
        )
    }
    step(t)
  }

  def iterate[X, E, F[_] : Monad, A](f: E => E, e: E): EnumeratorT[X, E, F, A] = {
    checkCont1[E, X, E, F, A](s => t => k => k(elInput(e)) >>== s(f(t)), e)
  }

  def repeat[X, E, F[_] : Monad, A](e: E): EnumeratorT[X, E, F, A] = {
    checkCont0[X, E, F, A](s => k => k(elInput(e)) >>== s)
  }

  def doneOr[X, O, I, F[_] : Pointed, A](f: (Input[I] => IterateeT[X, I, F, A]) => IterateeT[X, O, F, StepT[X, I, F, A]]): EnumerateeT[X, O, I, F, A] = {
    s =>
      def d: IterateeT[X, O, F, StepT[X, I, F, A]] = done(s, emptyInput)
      s.fold(
        cont = k => f(k)
        , done = (_, _) => d
        , err = _ => d
      )
  }
}

//
// Type class implementation traits
//

private[scalaz] trait EnumeratorTSemigroup[X, E, F[_], A] extends Semigroup[EnumeratorT[X, E, F, A]] {
  implicit def F: Bind[F]

  def append(f1: (StepT[X, E, F, A]) => IterateeT[X, E, F, A],
             f2: => (StepT[X, E, F, A]) => IterateeT[X, E, F, A]): (StepT[X, E, F, A]) => IterateeT[X, E, F, A] =
    s => f1(s) >>== f2
}

private[scalaz] trait EnumeratorTMonoid[X, E, F[_], A] extends Monoid[EnumeratorT[X, E, F, A]] with EnumeratorTSemigroup[X, E, F, A] {
  implicit def F: Monad[F]

  def zero: (StepT[X, E, F, A]) => IterateeT[X, E, F, A] = _.pointI
}

private[scalaz] trait EnumeratorTPlus[X, E, F[_]] extends Plus[({type λ[α]=EnumeratorT[X, E, F, α]})#λ] {
  implicit def F: Bind[F]

  def plus[A](f1: (StepT[X, E, F, A]) => IterateeT[X, E, F, A],
              f2: => (StepT[X, E, F, A]) => IterateeT[X, E, F, A]): (StepT[X, E, F, A]) => IterateeT[X, E, F, A] =
    s => f1(s) >>== f2
}

private[scalaz] trait EnumeratorTPlusEmpty[X, E, F[_]] extends PlusEmpty[({type λ[α]=EnumeratorT[X, E, F, α]})#λ] with EnumeratorTPlus[X, E, F] {
  implicit def F: Monad[F]

  def empty[A]: (StepT[X, E, F, A]) => IterateeT[X, E, F, A] = _.pointI
}
