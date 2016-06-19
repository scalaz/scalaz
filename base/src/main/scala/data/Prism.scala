package scalaz
package data

import typeclass.{Applicative, Choice}

import Disjunction._
import Maybe._

import Optic._

abstract class Prism[S, T, A, B] { self =>
  def apply[P[_, _], F[_]](pafb: P[A, F[B]])(implicit P: Choice[P], F: Applicative[F]): P[S, F[T]]

  // TODO
  // final def asOptic[P[_, _], F[_]](implicit P: Profunctor.Choice[P], F: Applicative[F]): Optic[P, F, S, T, A, B] = apply(_)

  final def asTraversal: Traversal[S, T, A, B] = new Traversal[S, T, A, B] {
    def apply[F[_]](afb: A => F[B])(implicit F: Applicative[F]): S => F[T] = self(afb)
  }
}

object Prism extends PrismFunctions
