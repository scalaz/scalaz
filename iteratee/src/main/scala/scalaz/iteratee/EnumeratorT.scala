package scalaz
package iteratee

import effect._

import IterateeT._, Input._

object EnumeratorT extends EnumeratorTFunctions with EnumeratorTInstances

trait EnumeratorTInstances0 {
  implicit def EnumeratorTSemigroup[X, E, F[_], A](implicit F0: Bind[F]) = new EnumeratorTSemigroup[X, E, F, A] {
    implicit def F = F0
  }
}


trait EnumeratorTInstances extends EnumeratorTInstances0 {
  implicit def EnumeratorTMonoid[X, E, F[_], A](implicit F0: Bind[F] with Pointed[F]) = new EnumeratorTMonoid[X, E, F, A] {
    implicit def F = F0
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
          import Ident.id
          i.fold(done = (_, _) => i, cont = k => enumerate(xs)(k(elInput(x)).value), err = e => err[Unit, A, Id, O](e).value)
      }

  implicit def enumStream[X, E, F[_] : Monad, A](xs: Stream[E]): EnumeratorT[X, E, F, A] = {
    s =>
      xs match {
        case h #:: t => s.mapContOr(_(elInput(h)) >>== enumStream(t), s.pointI)
        case _       => s.pointI
      }
  }

  implicit def enumIterator[X, E, A](x: Iterator[E]): EnumeratorT[X, E, IO, A] = {
    def loop: EnumeratorT[X, E, IO, A] = {
      s =>
        s.mapContOr(
          k =>
            if (x.hasNext) {
              val n = x.next
              k(elInput(n)) >>== loop
            } else s.pointI
          , s.pointI
        )
    }
    loop
  }

  import java.io._

  implicit def enumReader[X, A](r: Reader): EnumeratorT[X, IoExceptionOr[Char], IO, A] = {
    def loop: EnumeratorT[X, IoExceptionOr[Char], IO, A] = {
      s =>
        s.mapContOr(
          k => {
            val i = IoExceptionOr(r.read)
            if (i exists (_ != -1)) k(elInput(i.map(_.toChar))) >>== loop
            else s.pointI
          }
          , s.pointI
        )
    }
    loop
  }

  def checkCont0[X, E, F[_], A](z: EnumeratorT[X, E, F, A] => (Input[E] => IterateeT[X, E, F, A]) => IterateeT[X, E, F, A])(implicit p: Pointed[F]): EnumeratorT[X, E, F, A] = {
    def step: EnumeratorT[X, E, F, A] = {
      s =>
        s.mapContOr(
          k => z(step)(k)
          , s.pointI
        )
    }
    step
  }

  def checkCont1[S, X, E, F[_], A](z: (S => EnumeratorT[X, E, F, A]) => S => (Input[E] => IterateeT[X, E, F, A]) => IterateeT[X, E, F, A], t: S)(implicit p: Pointed[F]): EnumeratorT[X, E, F, A] = {
    def step: S => EnumeratorT[X, E, F, A] = {
      o => s =>
        s.mapContOr(
          k => z(step)(o)(k)
          , s.pointI
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

import EnumeratorT._

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
  implicit def F: Bind[F] with Pointed[F]

  def zero: (StepT[X, E, F, A]) => IterateeT[X, E, F, A] = _.pointI
}
