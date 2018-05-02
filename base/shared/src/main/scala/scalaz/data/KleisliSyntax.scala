package scalaz
package data

trait KleisliSyntax {
  import Kleisli.{ runKleisli, wrapKleisli }

  implicit class ToKleisliOps[F[_], A, B](k: Kleisli[F, A, B]) {

    def hoist[G[_]](η: F ~> G): Kleisli[G, A, B] =
      Kleisli.hoist(k)(η)

    def andThen[C](j: Kleisli[F, B, C])(implicit M: Monad[F]): Kleisli[F, A, C] =
      Kleisli.compose(j, k)

    def compose[E](j: Kleisli[F, E, A])(implicit M: Monad[F]): Kleisli[F, E, B] =
      Kleisli.compose(k, j)

    def >=>[C](j: Kleisli[F, B, C])(implicit M: Monad[F]): Kleisli[F, A, C] =
      Kleisli.compose(j, k)

    def <=<[E](j: Kleisli[F, E, A])(implicit M: Monad[F]): Kleisli[F, E, B] =
      Kleisli.compose(k, j)

    def >>>[C](j: Kleisli[F, B, C])(implicit M: Monad[F]): Kleisli[F, A, C] =
      Kleisli.compose(j, k)

    def =<<(fa: F[A])(implicit M: Monad[F]): F[B] =
      M.flatMap(fa)(runKleisli(k))

    def ***[C, D](j: Kleisli[F, C, D])(
      implicit S: Strong[Kleisli[F, ?, ?]],
      M: Monad[F]
    ): Kleisli[F, (A, C), (B, D)] =
      S.first(k) >>> S.second(j)

    def &&&[C](j: Kleisli[F, A, C])(
      implicit S: Strong[Kleisli[F, ?, ?]],
      M: Monad[F]
    ): Kleisli[F, A, (B, C)] =
      wrapKleisli((a: A) => M.pure((a, a))) >>> (k *** j)

  }

}
