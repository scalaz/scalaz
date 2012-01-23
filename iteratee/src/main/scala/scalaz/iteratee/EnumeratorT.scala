package scalaz
package iteratee

import effect._

import Iteratee._

trait EnumeratorT[X, E, F[_]] {
  def apply[A](step: StepT[X, E, F, A]): IterateeT[X, E, F, A]
}

trait EnumeratorTInstances0 {
  implicit def enumeratorTSemigroup[X, E, F[_]](implicit F0: Bind[F]): Semigroup[EnumeratorT[X, E, F]] = new EnumeratorTSemigroup[X, E, F] {
    implicit def F = F0
  }
}

trait EnumeratorTInstances extends EnumeratorTInstances0 {
  implicit def enumeratorTMonoid[X, E, F[_]](implicit F0: Monad[F]): Monoid[EnumeratorT[X, E, F]] = new EnumeratorTMonoid[X, E, F] {
    implicit def F = F0
  }
}

trait EnumeratorTFunctions {
  def enumerate[A, O](as: Stream[A]): Enumerator[Unit, A, O] =
    i =>
      as match {
        case Stream.Empty => i
        case x #:: xs     =>
          i.fold(done = (_, _) => i, cont = k => enumerate(xs)(k(elInput(x)).value), err = e => err[Unit, A, Id, O](e).value)
      }

  /** 
   * An EnumeratorT that is at EOF
   */
  def enumEofT[X, E, F[_] : Pointed]: EnumeratorT[X, E, F] =
    new EnumeratorT[X, E, F] { self =>
      def apply[A](s: StepT[X, E, F, A]) = s.mapCont(_(eofInput))
    }

  def enumOne[X, E, F[_]: Pointed, A](e: E): EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] {
      def apply[A](s: StepT[X, E, F, A]) = s.mapCont(_(elInput(e)))
    }

  implicit def enumStream[X, E, F[_] : Monad](xs: Stream[E]): EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] {
      def apply[A](s: StepT[X, E, F, A]) = xs match {
        case h #:: t => s.mapCont(k => k(elInput(h)) >>== enumStream(t).apply[A] _)
        case _       => s.pointI
      }
    }

  implicit def enumIterator[X, E](x: Iterator[E]): EnumeratorT[X, E, IO] = 
    new EnumeratorT[X, E, IO] { self =>
      def apply[A](s: StepT[X, E, IO, A]) = 
        s.mapCont(
          k =>
            if (x.hasNext) {
              val n = x.next()
              k(elInput(n)) >>== self[A]
            } else s.pointI
        )
    }

  import java.io._

  implicit def enumReader[X](r: Reader): EnumeratorT[X, IoExceptionOr[Char], IO] = 
    new EnumeratorT[X, IoExceptionOr[Char], IO] { self =>
      def apply[A](s: StepT[X, IoExceptionOr[Char], IO, A]) = 
        s.mapCont(
          k => {
            val i = IoExceptionOr(r.read)
            if (i exists (_ != -1)) k(elInput(i.map(_.toChar))) >>== self[A]
            else s.pointI
          }
        )
    }

  implicit def enumArray[X, E, F[_]: Monad](a : Array[E], min: Int = 0, max: Option[Int] = None) : EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] {
      private val limit = max.getOrElse(a.length)
      def apply[A](s: StepT[X, E, F, A]) = {
        def loop(pos : Int): StepT[X, E, F, A] => IterateeT[X, E, F, A] = {
          s => 
            s.mapCont(
              k => if (pos == limit) s.pointI
                   else              k(elInput(a(pos))) >>== loop(pos + 1)
            )   
        }

        loop(min)(s)
      }
    }

  def repeat[X, E, F[_] : Monad](e: E): EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] { self =>
      def apply[A](s: StepT[X, E, F, A]) = s.mapCont(_(elInput(e)) >>== self[A])
    }

  def iterate[X, E, F[_] : Monad](f: E => E, e: E): EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] { self =>
      def apply[A](s: StepT[X, E, F, A]): IterateeT[X, E, F, A] = {
        type StepM = StepT[X, E, F, A]
        type IterateeM = IterateeT[X, E, F, A]

        def checkCont1[S](z: (S => (StepM => IterateeM)) => S => (Input[E] => IterateeM) => IterateeM, lastState: S): (StepM => IterateeM) = {
          def step: S => (StepM => IterateeM) = {
            state => _.mapCont(k => z(step)(state)(k))
          }

          step(lastState)
        }

        checkCont1[E](contFactory => state => k => k(elInput(e)) >>== contFactory(f(state)), e)(s)
      }
    }
}

// Instances are mixed in with the IterateeT object
object EnumeratorT extends EnumeratorTFunctions with EnumeratorTInstances

//
// Type class implementation traits
//

private[scalaz] trait EnumeratorTSemigroup[X, E, F[_]] extends Semigroup[EnumeratorT[X, E, F]] {
  implicit def F: Bind[F]

  def append(f1: EnumeratorT[X, E, F], f2: => EnumeratorT[X, E, F]) = 
    new EnumeratorT[X, E, F] {
      def apply[A](s: StepT[X, E, F, A]) = f1[A](s) >>== f2[A]
    }
}

private[scalaz] trait EnumeratorTMonoid[X, E, F[_]] extends Monoid[EnumeratorT[X, E, F]] with EnumeratorTSemigroup[X, E, F] {
  implicit def F: Monad[F]

  def zero = new EnumeratorT[X, E, F] {
    def apply[A](s: StepT[X, E, F, A]) = s.pointI
  }
}

/*
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

private[scalaz] trait EnumeratorTFunctor[X, F[_], A] extends Functor[({type λ[α]=EnumeratorT[X, α, F, A]})#λ] {
  implicit def M: Monad[F]
  def map[E, B](fa: EnumeratorT[X, E, F, A])(f: E => B): EnumeratorT[X, B, F, A] = 
    (step: StepT[X, B, F, A]) => iterateeT[X, B, F, A](EnumerateeT.map[X, E, B, F, A](f).apply(step).run(x => err[X, B, F, A](x).value))
}

private[scalaz] trait EnumeratorTPointed[X, F[_], A] extends Pointed[({type λ[α]=EnumeratorT[X, α, F, A]})#λ] with EnumeratorTFunctor[X, F, A] {
  def point[E](e: => E) = EnumeratorT.enumOne[X, E, F, A](e)
}
*/
