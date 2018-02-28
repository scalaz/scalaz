package scalaz.data

trait KleisliModule {
  type Kleisli[M[_], A, B]

  def runKleisli[M[_], A, B](k: Kleisli[M, A, B]): A => M[B]
  def wrapKleisli[M[_], A, B](f: A => M[B]): Kleisli[M, A, B]
}

object KleisliImpl extends KleisliModule with KleisliInstances {
  type Kleisli[M[_], A, B] = A => M[B]

  def runKleisli[M[_], A, B](k: Kleisli[M, A, B]): A => M[B]  = k
  def wrapKleisli[M[_], A, B](k: A => M[B]): Kleisli[M, A, B] = k
}
