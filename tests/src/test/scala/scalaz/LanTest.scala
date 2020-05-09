package scalaz

object LanTest {

  object instances {
    def functor[F[_], G[_]] = Functor[Lan[F, G, *]]
    def apply[F[_]: Functor, G[_]: Apply] = Apply[Lan[F, G, *]]
    def applicative[F[_]: Functor, G[_]: Applicative] = Applicative[Lan[F, G, *]]

    // checking absence of ambiguity
    def functor[F[_]: Functor, G[_]: Apply] = Functor[Lan[F, G, *]]
    def functor[F[_]: Functor, G[_]: Applicative] = Functor[Lan[F, G, *]]
    def apply[F[_]: Functor, G[_]: Applicative] = Apply[Lan[F, G, *]]
  }

}
