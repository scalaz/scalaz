package scalaz

import Isomorphism._

/**
  * Takes functions operating on Bind and converts them into a BindRec version.
  *
  * Port of Phil Freeman's work in PureScript:
  *
  * https://github.com/paf31/purescript-safely/blob/ef7377d7f7659250f2f4a5da02c5d229e4eeed53/src/Control/Safely.purs#L19
  */
trait MonadRecSafely[M[_], A] {
  def apply[S[_]: BindRec: Applicative](lift: M <~> S): S[A]
}

object MonadRecSafely {
  def safely[M[_], A](f: MonadRecSafely[M, A])(implicit M: BindRec[M], MA: Applicative[M]): M[A] = {
    def iso[F[_]](implicit FBR: BindRec[F], FA: Applicative[F]): F <~> Free[F, ?] =
      new (F <~> Free[F, ?]) {
        val from: Free[F, ?] ~> F =
          new (Free[F, ?] ~> F) {
            def apply[A](fa: Free[F, A]): F[A] =
              fa.runRecM(a => a)(FA, FA, FBR)
          }
        val to: F ~> Free[F, ?] =
          new (F ~> Free[F, ?]) {
            def apply[A](fa: F[A]): Free[F, A] =
              Free.liftF(fa)
          }
      }

    iso[M].from(f(iso[M]))
  }
}
