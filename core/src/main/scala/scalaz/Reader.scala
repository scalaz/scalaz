package scalaz

object Reader extends ReaderInstances with ReaderFunctions {
  type ReaderT[E, F[_], A] = E => F[A]
  type Reader[E, A] = E => A

}

trait ReaderInstances {
  import Reader._

  implicit def reader[S]: MonadReader[({type f[s, a] = Reader[s, a]})#f, S] =
    readerTInstance[S, Id](Ident.id)
  implicit def readerT[S, F[_] : Monad] = readerTInstance[S, F]:
    MonadReader[({type f[s, a] = ReaderT[s, F, a]})#f, S] with
      Monad[({type f[a] = ReaderT[S, F, a]})#f] with
      Applicative[({type f[a] = ReaderT[S, F, a]})#f] with
      Apply[({type f[a] = ReaderT[S, F, a]})#f] with
      Bind[({type f[a] = ReaderT[S, F, a]})#f]

  private def readerTInstance[S, F[_]](implicit F: Monad[F]) =
    new MonadReader[({type f[s, a] = ReaderT[s, F, a]})#f, S] {
      def pure[A](a: => A): ReaderT[S, F, A] = s => F.pure(a)
      override def map[A, B](fa: ReaderT[S, F, A])(f: A => B): ReaderT[S, F, B] = fa andThen F(f)
      def bind[A, B](fa: ReaderT[S, F, A])(f: A => ReaderT[S, F, B]): ReaderT[S, F, B] =
        s => F.bind(fa(s))(a => f(a)(s))
      def ask: ReaderT[S, F, S] = s => F.pure(s)
      def local[A](f: S => S)(fa: ReaderT[S, F, A]) = fa compose f
    }

  trait ReaderTF[S, G[_]] {
    type f[x] = ReaderT[S, G, x]
  }

  implicit def readerMT[S] = new MonadTrans[({type f[g[_], a] = ReaderT[S, g, a]})#f] {
    def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): ReaderT[S, G, A] =
      s => G.map(ga)(a => a)
    def hoist[M[_], N[_]](f: M ~> N) = new (ReaderTF[S, M]#f ~> ReaderTF[S, N]#f) {
      def apply[A](action: ReaderT[S, M, A]) = ((s: S) => f(action(s)))
    }
  }
}

trait ReaderFunctions {
}
