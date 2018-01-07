package scalaz
package data

trait FreeModule {
  type Free[F[_], A]

  def runFree[F[_], A](f: Free[F, A]): F[Free[F, A]]

  def wrapFree[F[_], A](f: Kleisli[F, A, data.Free[F, A]]): Free[F, A]
}

private[data] object FreeImpl extends FreeModule {
  type Free[F[_], A] = ACatenable1[Kleisli[F, ?, ?], A, data.Free[F, A]]

  def runFree[F[_], A](f: Free[F, A]): F[Free[F, A]] = ???

  def wrapFree[F[_], A](f: Kleisli[F, A, data.Free[F, A]]): Free[F, A] =
    ACatenable1.lift[Kleisli[F, ?, ?], A, data.Free[F, A]](f)
}
