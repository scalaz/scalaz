package scalaz.data

import scalaz.Bind

trait KleisliFunctions {
  @inline def wrap[F[_], A, B](k: A => F[B]): Kleisli[F, A, B] = Kleisli.wrap(k)
  @inline def run[F[_], A, B](k: Kleisli[F, A, B]): A => F[B]  = Kleisli.run(k)
  @inline def compose[F[_], A, B, C](k: Kleisli[F, A, B], j: Kleisli[F, B, C])(implicit B: Bind[F]): Kleisli[F, A, C] =
    Kleisli.compose(k, j)

}
