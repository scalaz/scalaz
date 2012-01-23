package scalaz
package iteratee

import effect._

import Iteratee._

trait EnumeratorT[X, E, F[_]] {
  def apply[A]: StepT[X, E, F, A] => IterateeT[X, E, F, A]
}

trait EnumeratorTInstances0 {
  implicit def enumeratorTSemigroup[X, E, F[_]](implicit F0: Bind[F]): Semigroup[EnumeratorT[X, E, F]] = new EnumeratorTSemigroup[X, E, F] {
    implicit def F = F0
  }

  implicit def enumeratorTFunctor[X, F[_]](implicit M0: Monad[F]): Functor[({type λ[α]=EnumeratorT[X, α, F]})#λ] = new EnumeratorTFunctor[X, F] {
    implicit def M = M0
  }
}

trait EnumeratorTInstances extends EnumeratorTInstances0 {
  implicit def enumeratorTMonoid[X, E, F[_]](implicit F0: Monad[F]): Monoid[EnumeratorT[X, E, F]] = new EnumeratorTMonoid[X, E, F] {
    implicit def F = F0
  }

  implicit def enumeratorTPointed[X, F[_]](implicit M0: Monad[F]): Pointed[({type λ[α]=EnumeratorT[X, α, F]})#λ] = new EnumeratorTPointed[X, F] {
    implicit def M = M0
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
    new EnumeratorT[X, E, F] { 
      def apply[A] = _.mapCont(_(eofInput))
    }

  def enumOne[X, E, F[_]: Pointed](e: E): EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] {
      def apply[A] = _.mapCont(_(elInput(e)))
    }

  implicit def enumStream[X, E, F[_] : Monad](xs: Stream[E]): EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] {
      def apply[A] = (s: StepT[X, E, F, A]) => xs match {
        case h #:: t => s.mapCont(k => k(elInput(h)) >>== enumStream(t).apply[A])
        case _       => s.pointI
      }
    }

  implicit def enumIterator[X, E](x: Iterator[E]): EnumeratorT[X, E, IO] = 
    new EnumeratorT[X, E, IO] { 
      def apply[A] = (s: StepT[X, E, IO, A]) => 
        s.mapCont(
          k =>
            if (x.hasNext) {
              val n = x.next()
              k(elInput(n)) >>== apply[A]
            } else s.pointI
        )
    }

  import java.io._

  implicit def enumReader[X](r: Reader): EnumeratorT[X, IoExceptionOr[Char], IO] = 
    new EnumeratorT[X, IoExceptionOr[Char], IO] { 
      def apply[A] = (s: StepT[X, IoExceptionOr[Char], IO, A]) => 
        s.mapCont(
          k => {
            val i = IoExceptionOr(r.read)
            if (i exists (_ != -1)) k(elInput(i.map(_.toChar))) >>== apply[A]
            else s.pointI
          }
        )
    }

  implicit def enumArray[X, E, F[_]: Monad](a : Array[E], min: Int = 0, max: Option[Int] = None) : EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] {
      private val limit = max.getOrElse(a.length)
      def apply[A] = {
        def loop(pos : Int): StepT[X, E, F, A] => IterateeT[X, E, F, A] = {
          s => 
            s.mapCont(
              k => if (pos == limit) s.pointI
                   else              k(elInput(a(pos))) >>== loop(pos + 1)
            )   
        }

        loop(min)
      }
    }

  def repeat[X, E, F[_] : Monad](e: E): EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] { 
      def apply[A] = (s: StepT[X, E, F, A]) => s.mapCont(_(elInput(e)) >>== apply[A])
    }

  def iterate[X, E, F[_] : Monad](f: E => E, e: E): EnumeratorT[X, E, F] = 
    new EnumeratorT[X, E, F] { 
      def apply[A]: StepT[X, E, F, A] => IterateeT[X, E, F, A] = {
        type StepM = StepT[X, E, F, A]
        type IterateeM = IterateeT[X, E, F, A]

        def checkCont1(z: (E => (StepM => IterateeM)) => E => (Input[E] => IterateeM) => IterateeM, lastState: E): (StepM => IterateeM) = {
          def step: E => (StepM => IterateeM) = {
            state => _.mapCont(k => z(step)(state)(k))
          }

          step(lastState)
        }

        checkCont1(contFactory => state => k => k(elInput(e)) >>== contFactory(f(state)), e)
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
      def apply[A] = (s: StepT[X, E, F, A]) => f1[A](s) >>== f2[A]
    }
}

private[scalaz] trait EnumeratorTMonoid[X, E, F[_]] extends Monoid[EnumeratorT[X, E, F]] with EnumeratorTSemigroup[X, E, F] {
  implicit def F: Monad[F]

  def zero = new EnumeratorT[X, E, F] {
    def apply[A] = (s: StepT[X, E, F, A]) => s.pointI
  }
}

private[scalaz] trait EnumeratorTFunctor[X, F[_]] extends Functor[({type λ[α]=EnumeratorT[X, α, F]})#λ] {
  implicit def M: Monad[F]
  def map[A, B](fa: EnumeratorT[X, A, F])(f: A => B): EnumeratorT[X, B, F] = 
    new EnumeratorT[X, B, F] {
      def apply[C] = { (step: StepT[X, B, F, C]) => 
        iterateeT((EnumerateeT.map[X, A, B, F](f).apply(step) &= fa).run(x => err[X, B, F, C](x).value))
      }
    }
}

private[scalaz] trait EnumeratorTPointed[X, F[_]] extends Pointed[({type λ[α]=EnumeratorT[X, α, F]})#λ] with EnumeratorTFunctor[X, F] {
  def point[E](e: => E) = EnumeratorT.enumOne[X, E, F](e)
}
