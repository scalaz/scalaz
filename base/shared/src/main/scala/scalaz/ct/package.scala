package scalaz

package object ct {

  val Kleisli: KleisliModule = KleisliImpl
  type Kleisli[F[_], A, B] = Kleisli.Kleisli[F, A, B]
}
