package scalaz

import scalaz._
import Scalaz._

trait InvariantFunctor[F[_]] {
  def xmap[A, B](ma: F[A], f: A => B, g: B => A): F[B]
}

object InvariantFunctor {
  implicit val MonoidInvariantFunctor = new InvariantFunctor[Monoid] {
    def xmap[A, B](ma: Monoid[A], f: A => B, g: B => A): Monoid[B] = new Monoid[B] {
      def append(b1: B, b2: => B) = f(ma append (g(b1), g(b2)))

      val zero = f(ma.zero)
    }
  }
  
  implicit val EndoInvariantFunctor = new InvariantFunctor[Endo] {
    def xmap[A, B](ma: Endo[A], f: A => B, g: B => A): Endo[B] = {
      (b: B) => f(ma(g(b)))
    }
  }

  implicit val SemigroupInvariantFunctor = new InvariantFunctor[Semigroup] {
    def xmap[A, B](ma: Semigroup[A], f: A => B, g: B => A): Semigroup[B] = new Semigroup[B] {
      def append(b1: B, b2: => B): B = f(ma append (g(b1), g(b2)))
    }
  }
  
}
