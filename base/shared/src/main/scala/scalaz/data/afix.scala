package scalaz
package data

trait AFixModule {
  type AFix[F[_[_, _], _, _], A, B]

  def fix[F[_[_, _], _, _], A, B](f: F[data.AFix[F, ?, ?], A, B]): AFix[F, A, B]
  def unfix[F[_[_, _], _, _], A, B](f: AFix[F, A, B]): F[data.AFix[F, ?, ?], A, B]
}

private[data] object AFixImpl extends AFixModule {
  type AFix[F[_[_, _], _, _], A, B] = F[data.AFix[F, ?, ?], A, B]

  def fix[F[_[_, _], _, _], A, B](f: F[data.AFix[F, ?, ?], A, B]): AFix[F, A, B]   = f
  def unfix[F[_[_, _], _, _], A, B](f: AFix[F, A, B]): F[data.AFix[F, ?, ?], A, B] = f
}
