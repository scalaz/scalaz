package scalaz

object LanTest {

  object instances {
    def functor[F[_], G[_]] = Functor[({type l[a] = Lan[F, G, a]})#l]
    def apply[F[_]: Functor, G[_]: Apply] = Apply[({type l[a] = Lan[F, G, a]})#l]
    def applicative[F[_]: Functor, G[_]: Applicative] = Applicative[({type l[a] = Lan[F, G, a]})#l]

    // checking absence of ambiguity
    def functor[F[_]: Functor, G[_]: Apply] = Functor[({type l[a] = Lan[F, G, a]})#l]
    def functor[F[_]: Functor, G[_]: Applicative] = Functor[({type l[a] = Lan[F, G, a]})#l]
    def apply[F[_]: Functor, G[_]: Applicative] = Apply[({type l[a] = Lan[F, G, a]})#l]
  }

}
