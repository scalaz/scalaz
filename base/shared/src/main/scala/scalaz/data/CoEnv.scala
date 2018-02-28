package scalaz.data

import scalaz.data.Disjunction.\/

trait CoEnvModule {
  type CoEnv[F[_], A, B]

  def run[F[_], A, B](coEnv: CoEnv[F, A, B]): B \/ F[A]
  def wrapCoEnv[F[_], A, B](e: B \/ F[A]): CoEnv[F, A, B]
}

private[data] object CoEnvImpl extends CoEnvModule {
  type CoEnv[F[_], A, B] = B \/ F[A]

  def run[F[_], A, B](c: CoEnv[F, A, B]): B \/ F[A]       = c
  def wrapCoEnv[F[_], A, B](c: B \/ F[A]): CoEnv[F, A, B] = c
}
