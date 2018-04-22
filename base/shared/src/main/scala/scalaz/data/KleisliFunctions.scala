package scalaz
package data

trait KleisliFunctions {
  @inline def wrap[F[_], A, B](k: A => F[B]): Kleisli[F, A, B] = Kleisli.wrap(k)
  @inline def run[F[_], A, B](k: Kleisli[F, A, B]): A => F[B]  = Kleisli.run(k)
}
