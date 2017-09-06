package scalaz
package data

import Prelude._
import typeclass.Liskov
import typeclass.Liskov.<~<

trait FixModule {
  type Fix[F[_]]

  /* functions */

  def   fix[F[_]](f: F[Fix[F]]):   Fix[F]
  def unfix[F[_]](f:   Fix[F] ): F[Fix[F]]

  def liftLiskov[F[_], G[_]](ev: ∀[λ[α => F[α] <~< G[α]]])(implicit F: IsCovariant[F]): Fix[F] <~< Fix[G]
}

private[data] trait FixImpl extends FixModule {
  type Fix[F[_]] = F[Any]

  def   fix[F[_]](f: F[Fix[F]]):   Fix[F]  = f.asInstanceOf[F[Any]]
  def unfix[F[_]](f:   Fix[F] ): F[Fix[F]] = f.asInstanceOf[F[Fix[F]]]

  def liftLiskov[F[_], G[_]](ev: ∀[λ[α => F[α] <~< G[α]]])(implicit F: IsCovariant[F]): Fix[F] <~< Fix[G] =
    Liskov.unsafeForce
}
