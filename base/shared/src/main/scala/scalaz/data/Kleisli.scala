package scalaz.data

trait KleisliModule {
  type Kleisli[M[_], A, B]

  def runKleisli[M[_], A, B](k: Kleisli[M, A, B]): A => M[B]
  def wrapKleisli[M[_], A, B](f: A => M[B]): Kleisli[M, A, B]
}

private[data] object KleisliImpl extends KleisliModule {
  type Kleisli[M[_], A, B] = A => M[B]

  def runKleisli[M[_], A, B](k: Kleisli[M, A, B]): A => M[B] = k
  def wrapKleisli[M[_], A, B](f: A => M[B]): Kleisli[M, A, B] = f
}
