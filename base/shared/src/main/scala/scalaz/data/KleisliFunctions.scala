package scalaz
package data

trait KleisliFunctions {
  @inline def wrapKleisli[F[_], A, B](k: A => F[B]): Kleisli[F, A, B] = Kleisli.wrapKleisli(k)
  @inline def runKleisli[F[_], A, B](k: Kleisli[F, A, B]): A => F[B]  = Kleisli.runKleisli(k)
}
