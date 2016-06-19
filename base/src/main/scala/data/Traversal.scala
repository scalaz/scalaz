package scalaz
package data

import typeclass.Applicative

abstract class Traversal[S, T, A, B] {
  def apply[F[_]](afb: A => F[B])(implicit F: Applicative[F]): S => F[T]

  def ∘[I, O](i: I)(implicit Compose: Traversal.Compose[I, O, S, T, A, B]): O = Compose(this)(i)
}

object Traversal {
  trait Compose[I, O, S, T, A, B] { def apply(lens: Traversal[S, T, A, B]): I => O }

  object Compose {
    implicit def lens[S, T, A, B, C, D]: Compose[Lens[A, B, C, D], Traversal[S, T, C, D], S, T, A, B] =
      new Compose[Lens[A, B, C, D], Traversal[S, T, C, D], S, T, A, B] {
        def apply(stab: Traversal[S, T, A, B]): Lens[A, B, C, D] => Traversal[S, T, C, D] = abcd => new Traversal[S, T, C, D] {
          def apply[F[_]](afb: C => F[D])(implicit F: Applicative[F]): S => F[T] = stab.apply(abcd(afb)(F.apply.functor))
        }
      }
    implicit def traversal[S, T, A, B, C, D]: Compose[Traversal[A, B, C, D], Traversal[S, T, C, D], S, T, A, B] =
      new Compose[Traversal[A, B, C, D], Traversal[S, T, C, D], S, T, A, B] {
        def apply(stab: Traversal[S, T, A, B]): Traversal[A, B, C, D] => Traversal[S, T, C, D] = abcd => new Traversal[S, T, C, D] {
          def apply[F[_]](afb: C => F[D])(implicit F: Applicative[F]): S => F[T] = stab.apply(abcd(afb))
        }
      }
  }
}
