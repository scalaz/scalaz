package scalaz
package data

import typeclass.{Functor, Profunctor}

trait DownStarInstances {
  implicit def downstarProfunctor[F[_]](implicit F: Functor[F]): Profunctor[DownStar[F, ?, ?]] =
    new Profunctor[DownStar[F, ?, ?]] {
      override def lmap[A, B, C](fab: DownStar[F, A, B])(ca: C => A): DownStar[F, C, B] =
        DownStar(fc => fab.run(F.map(fc)(ca)))

      override def rmap[A, B, C](fab: DownStar[F, A, B])(bc: B => C): DownStar[F, A, C] =
        DownStar(a => bc(fab.run(a)))

      override def dimap[A, B, C, D](fab: DownStar[F, A, B])(ca: C => A)(bd: B => D): DownStar[F, C, D] =
        DownStar(fc => bd(fab.run(F.map(fc)(ca))))
    }
}
