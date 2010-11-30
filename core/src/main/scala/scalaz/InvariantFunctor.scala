package scalaz

import scalaz._
import Scalaz._

trait InvariantFunctor[F[_]] {
  def xmap[A, B](ma: F[A], f: A => B, g: B => A): F[B]
}

object InvariantFunctor {
  implicit val MonoidInvariantFunctor: InvariantFunctor[Monoid] = new InvariantFunctor[Monoid] {
    def xmap[A, B](ma: Monoid[A], f: A => B, g: B => A): Monoid[B] = new Monoid[B] {
      def append(b1: B, b2: => B) = f(ma append (g(b1), g(b2)))

      val zero = f(ma.zero)
    }
  }

  // The type ascription ': InvariantFunctor[Endo]' works around the following spurious compiler error in March 2010 vintage
  // compiler builds: "not found: type $anon"
  //
  // Reported: https://lampsvn.epfl.ch/trac/scala/ticket/3177
  implicit val EndoInvariantFunctor: InvariantFunctor[Endo] = new InvariantFunctor[Endo] { //
    def xmap[A, B](ma: Endo[A], f: A => B, g: B => A): Endo[B] = EndoTo((b: B) => f(ma(g(b))))
  }

  implicit val SemigroupInvariantFunctor: InvariantFunctor[Semigroup] = new InvariantFunctor[Semigroup] {
    def xmap[A, B](ma: Semigroup[A], f: A => B, g: B => A): Semigroup[B] = new Semigroup[B] {
      def append(b1: B, b2: => B): B = f(ma append (g(b1), g(b2)))
    }
  }

  implicit def MemoInvariantFunctor[V]: InvariantFunctor[({type λ[α]=Memo[α, V]})#λ] = new InvariantFunctor[({type λ[α]=Memo[α, V]})#λ] {
    def xmap[A, B](ma: Memo[A, V], f: A => B, g: B => A): Memo[B, V] = {
      memo {
        (h: B => V) =>
          val m = ma { h ∙ f }
          (b: B) => m(g(b))
      }
    }
  }

}
