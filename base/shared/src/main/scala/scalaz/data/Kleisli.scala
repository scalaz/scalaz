package scalaz.data

sealed trait KleisliModule {
  type Kleisli[M[_], A, B]

  def runKleisli[M[_], A, B](k: Kleisli[M, A, B]): A => M[B]
}

private[data] object KleisliImpl extends KleisliModule {
  type Kleisli[M[_], A, B] = A => M[B]

  def runKleisli[M[_], A, B](k: Kleisli[M, A, B]): A => M[B] = k
}
