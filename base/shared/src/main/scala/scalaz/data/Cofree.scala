package scalaz
package data

import scalaz.control.Inf

trait CofreeModule {
  type Cofree[F[_], A]

  def runCofree[F[_], A](f: Cofree[F, A]): (A, Inf[F[Cofree[F, A]]])

  def wrapCofree[F[_], A](a: =>A)(f: =>F[Cofree[F, A]]): Cofree[F, A]
}

private[data] final class MkCofree[A, T[_], X](val runCofree: (A, Inf[T[X]])) extends AnyVal

private[data] object CofreeImpl extends CofreeModule {

  type Cofree[F[_], A] = Fix[MkCofree[A, F, ?]]

  def runCofree[F[_], A](f: Cofree[F, A]): (A, Inf[F[Cofree[F, A]]]) =
    Fix.unfix[MkCofree[A, F, ?]](f).runCofree

  def wrapCofree[F[_], A](a: =>A)(f: =>F[Cofree[F, A]]): Cofree[F, A] =
    Fix.fix[MkCofree[A, F, ?]](new MkCofree((a, Inf(f))))
}
