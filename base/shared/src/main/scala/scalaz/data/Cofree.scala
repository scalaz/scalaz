package scalaz
package data

import scalaz.control.{Coinductive, Inf}

trait CofreeModule {
  type Cofree[F[_], A]

  def runCofree[F[_], A](f: Cofree[F, A]): F[data.Cofree[F, A]]

  def wrapCofree[F[_], A](a: =>A)(f: =>F[data.Cofree[F, A]]): Cofree[F, A]
}

private[data] object CofreeImpl extends CofreeModule {
  type Cofree[F[_], A] = (Coinductive[A], Coinductive[F[data.Cofree[F, A]]])

  def runCofree[F[_], A](f: Cofree[F, A]): F[data.Cofree[F, A]] =
    f._2.force

  def wrapCofree[F[_], A](a: =>A)(f: =>F[data.Cofree[F, A]]): Cofree[F, A] =
    (Coinductive(Inf(a)), Coinductive(Inf(f)))
}
