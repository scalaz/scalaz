package scalaz
package data

import Prelude._

trait KleisliSyntax {
  import Kleisli.{ runKleisli, wrapKleisli }

  implicit class ToKleisliOps[F[_], A, B](k: Kleisli[F, A, B]) {

    def hoist[G[_]](η: F ~> G): Kleisli[G, A, B] =
      Kleisli.hoist(k)(η)

    def andThen[C](j: Kleisli[F, B, C])(implicit B: Bind[F]): Kleisli[F, A, C] =
      Kleisli.compose(j, k)

    def compose[E](j: Kleisli[F, E, A])(implicit B: Bind[F]): Kleisli[F, E, B] =
      Kleisli.compose(k, j)

    def first[C](implicit F: Functor[F]): Kleisli[F, (A, C), (B, C)] =
      Kleisli.first(k)

    def second[C](implicit F: Functor[F]): Kleisli[F, (C, A), (C, B)] =
      Kleisli.second(k)

    def >=>[C](j: Kleisli[F, B, C])(implicit B: Bind[F]): Kleisli[F, A, C] =
      Kleisli.compose(j, k)

    def <=<[E](j: Kleisli[F, E, A])(implicit B: Bind[F]): Kleisli[F, E, B] =
      Kleisli.compose(k, j)

    def >>>[C](j: Kleisli[F, B, C])(implicit B: Bind[F]): Kleisli[F, A, C] =
      Kleisli.compose(j, k)

    def =<<(fa: F[A])(implicit B: Bind[F]): F[B] =
      B.flatMap(fa)(runKleisli(k))

    def ***[C, D](j: Kleisli[F, C, D])(
      implicit B: Bind[F]
    ): Kleisli[F, (A, C), (B, D)] =
      k.first >>> j.second

    // a => F[(a, a)] => ((a, a) => F[(b, c)])
    def &&&[C](j: Kleisli[F, A, C])(
      implicit M: Monad[F]
    ): Kleisli[F, A, (B, C)] =
      wrapKleisli((a: A) => M.pure((a, a))) >>> (k *** j)

  }
}
