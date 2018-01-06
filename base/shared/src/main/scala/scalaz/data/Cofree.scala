package scalaz
package data

import scalaz.control.Inf

sealed trait CofreeModule {
  type Cofree[F[_], A]

  def runCofree[F[_], A](f: Cofree[F, A]): (Inf[A], Inf[F[Cofree[F, A]]])

  def wrapCofree[F[_], A](a: =>A)(f: =>F[Cofree[F, A]]): Cofree[F, A]
}


private[data] object CofreeImpl extends CofreeModule {

  type Cofree[F[_], A] = Fix[EnvT[A, F, ?]]

  def runCofree[F[_], A](f: Cofree[F, A]): (Inf[A], Inf[F[Cofree[F, A]]]) =
    Fix.unfix[EnvT[A, F, ?]](f).run

  def wrapCofree[F[_], A](a: =>A)(f: =>F[Cofree[F, A]]): Cofree[F, A] =
    Fix.fix[EnvT[A, F, ?]](new EnvT((Inf.apply(a), Inf.apply(f))))
}
