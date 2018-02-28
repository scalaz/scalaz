package scalaz.data

trait EnvTModule {
  type EnvT[T[_], A, B]

  def run[T[_], A, B](env: EnvT[T, A, B]): (A, T[B])
  def wrapEnvT[T[_], A, B](env: (A, T[B])): EnvT[T, A, B]
}

private[data] object EnvTImpl extends EnvTModule {
  type EnvT[T[_], A, B] = (A, T[B])

  def run[T[_], A, B](env: EnvT[T, A, B]): (A, T[B])      = env
  def wrapEnvT[T[_], A, B](env: (A, T[B])): EnvT[T, A, B] = env
}
