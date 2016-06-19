package scalaz
package data

import typeclass.{Applicative, Choice}

import Disjunction._
import Maybe._
import Optic._

trait PrismFunctions {
  def prism[S, T, A, B](bt: B => T)(seta: S => T \/ A): Prism[S, T, A, B] = new Prism[S, T, A, B] {
    def apply[P[_, _], F[_]](pafb: P[A, F[B]])(implicit P: Choice[P], F: Applicative[F]): P[S, F[T]] =
      P.profunctor.dimap(P.right[A, F[B], T](pafb))(seta)(either(F.pure[T])(F.apply.functor.map[B, T](_)(bt)))
  }

  def prismM[S, A, B](bs: B => S)(sma: S => Maybe[A]): Prism[S, S, A, B] =
    prism[S, S, A, B](bs)(s => maybe[A, S \/ A](-\/(s))(\/-(_))(sma(s)))
}
