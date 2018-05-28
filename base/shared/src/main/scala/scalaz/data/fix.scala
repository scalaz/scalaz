package scalaz
package data

import scalaz.types.As

trait FixModule {
  type Fix[F[_]]

  /* functions */

  def fix[F[_]](f: F[data.Fix[F]]): Fix[F]
  def unfix[F[_]](f: Fix[F]): F[data.Fix[F]]

  def subst[G[_[_[_]]]](g: G[λ[α[_] => α[data.Fix[α]]]]): G[Fix]

  def liftLiskov[F[_], G[_]](ev: ∀[λ[α => F[α] <~< G[α]]])(implicit F: IsCovariant[F]): Fix[F] <~< Fix[G]
}

private[data] object FixImpl extends FixModule {
  type Fix[F[_]] = F[data.Fix[F]]

  def fix[F[_]](f: F[data.Fix[F]]): Fix[F]   = f
  def unfix[F[_]](f: Fix[F]): F[data.Fix[F]] = f

  def subst[G[_[_[_]]]](g: G[λ[α[_] => α[data.Fix[α]]]]): G[Fix] = g

  def liftLiskov[F[_], G[_]](ev: ∀[λ[α => F[α] <~< G[α]]])(implicit F: IsCovariant[F]): Fix[F] <~< Fix[G] =
    As.unsafeForce
}
