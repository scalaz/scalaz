package scalaz
package data

trait KleisliSyntax {
  import Kleisli.{ run, wrap }

  implicit class ToKleisliOps[F[_], A, B](k: Kleisli[F, A, B]) {

    def hoist[G[_]](η: F ~> G): Kleisli[G, A, B] =
      Kleisli.hoist(k)(η)

    def andThen[C](
      j: Kleisli[F, B, C]
    )(implicit M: Monad[F]): Kleisli[F, A, C] =
      Kleisli.compose(k, j)

    def compose[E](
      j: Kleisli[F, E, A]
    )(implicit M: Monad[F]): Kleisli[F, E, B] =
      Kleisli.compose(j, k)

    def >=>[C](j: Kleisli[F, B, C])(implicit M: Monad[F]): Kleisli[F, A, C] =
      Kleisli.compose(k, j)

    def <=<[E](j: Kleisli[F, E, A])(implicit M: Monad[F]): Kleisli[F, E, B] =
      k compose j

    def =<<(fa: F[A])(implicit M: Monad[F]): F[B] =
      M.flatMap(fa)(run(k))

    def ***[C, D](j: Kleisli[F, C, D])(
      implicit S: Strong[Kleisli[F, ?, ?]],
      M: Monad[F]
    ): Kleisli[F, (A, C), (B, D)] =
      S.first(k) >=> S.second(j)

    def &&&[C](j: Kleisli[F, A, C])(
      implicit S: Strong[Kleisli[F, ?, ?]],
      M: Monad[F]
    ): Kleisli[F, A, (B, C)] =
      wrap((a: A) => M.pure((a, a))) >=> (k *** j)

  }

}
