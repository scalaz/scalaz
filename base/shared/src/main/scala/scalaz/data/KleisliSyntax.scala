package scalaz
package data

trait KleisliSyntax {
  import Kleisli.run

  implicit class Ops[F[_], A, B](k: Kleisli[F, A, B]) {

    def hoist[G[_]](η: F ~> G): Kleisli[G, A, B] =
      Kleisli.hoist(k)(η)

    def >=>[C](j: Kleisli[F, B, C])(implicit B: Bind[F]): Kleisli[F, A, C] =
      Kleisli.compose(k, j)

    def <=<[C](j: Kleisli[F, C, A])(implicit b: Bind[F]): Kleisli[F, C, B] =
      j >=> k

    def =<<(fa: F[A])(implicit B: Bind[F]): F[B] =
      B.flatMap(fa)(run(k).apply)
  }

}
