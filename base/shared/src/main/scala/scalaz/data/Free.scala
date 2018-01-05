package scalaz
package data

trait FreeModule {
  type Free[F[_], A]

  def runFree[F[_], A](f: Free[F, A]): A \/ F[data.Free[F, A]]

  def wrapFree[F[_], A](f: A \/ F[data.Free[F, A]]): Free[F, A]
}

private[data] object FreeImpl extends FreeModule {
  type Free[F[_], A] = A \/ F[data.Free[F, A]]

  def runFree[F[_], A](f: Free[F, A]): A \/ F[data.Free[F, A]] = f

  def wrapFree[F[_], A](f: A \/ F[data.Free[F, A]]): Free[F, A] = f
}
