package scalaz
package data

sealed trait CofreeModule {
  type Cofree[F[_], A]

  def runCofree[F[_], A](f: Cofree[F, A]): (A, F[Cofree[F, A]])

  def wrapCofree[F[_], A](a: A)(f: F[Cofree[F, A]]): Cofree[F, A]
}

private[data] object CofreeImpl extends CofreeModule {

  type Cofree[F[_], A] = Fix[EnvT[F, A, ?]]

  def runCofree[F[_], A](f: Cofree[F, A]): (A, F[Cofree[F, A]]) =
    EnvT.run[F, A, Cofree[F, A]](Fix.unfix[EnvT[F, A, ?]](f))

  def wrapCofree[F[_], A](a: A)(f: F[Cofree[F, A]]): Cofree[F, A] =
    Fix.fix[EnvT[F, A, ?]](EnvT.wrapEnvT((a, f)))
}
