package scalaz

// TODO decide between this and the ReaderT wrapper class
//      Maybe move this over to Function2.
object ReaderAlias extends ReaderAliasInstances {
  type ReaderAliasT[E, F[_], A] = E => F[A]
  type ReaderAlias[E, A] = E => A

}

trait ReaderAliasInstances {
  import ReaderAlias._

  implicit def reader[S]: MonadReader[({type f[s, a] = ReaderAlias[s, a]})#f, S] =
    readerTInstance[S, Id](Id.id)
  implicit def readerT[S, F[_] : Monad] = readerTInstance[S, F]:
    MonadReader[({type f[s, a] = ReaderAliasT[s, F, a]})#f, S] with
      Monad[({type f[a] = ReaderAliasT[S, F, a]})#f] with
      Applicative[({type f[a] = ReaderAliasT[S, F, a]})#f] with
      Apply[({type f[a] = ReaderAliasT[S, F, a]})#f] with
      Bind[({type f[a] = ReaderAliasT[S, F, a]})#f]

  private def readerTInstance[S, F[_]](implicit F: Monad[F]) =
    new MonadReader[({type f[s, a] = ReaderAliasT[s, F, a]})#f, S] {
      def point[A](a: => A): ReaderAliasT[S, F, A] = s => F.point(a)
      override def map[A, B](fa: ReaderAliasT[S, F, A])(f: A => B): ReaderAliasT[S, F, B] = fa andThen F(f)
      def bind[A, B](fa: ReaderAliasT[S, F, A])(f: A => ReaderAliasT[S, F, B]): ReaderAliasT[S, F, B] =
        s => F.bind(fa(s))(a => f(a)(s))
      def ask: ReaderAliasT[S, F, S] = s => F.point(s)
      def local[A](f: S => S)(fa: ReaderAliasT[S, F, A]) = fa compose f
    }

  trait ReaderTF[S, G[_]] {
    type f[x] = ReaderAliasT[S, G, x]
  }

  implicit def readerMT[S] = new MonadTrans[({type f[g[_], a] = ReaderAliasT[S, g, a]})#f] {
    def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): ReaderAliasT[S, G, A] =
      s => G.map(ga)(a => a)
    def hoist[M[_]: Monad, N[_]](f: M ~> N) = new (ReaderTF[S, M]#f ~> ReaderTF[S, N]#f) {
      def apply[A](action: ReaderAliasT[S, M, A]) = ((s: S) => f(action(s)))
    }
  }
}
