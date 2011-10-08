package scalaz


trait StateTsLow2 {
  implicit def stateTFunctor[S, F[_]](implicit F0: Functor[F]) = new StateTFunctor[S, F] {
    implicit def F: Functor[F] = F0
  }
}

trait StateTsLow1 extends StateTsLow2 {
  implicit def stateTPointed[S, F[_]](implicit F0: Pointed[F]) = new StateTPointed[S, F] {
    implicit def F: Pointed[F] = F0
  }
}

trait StateTs extends StateTsLow1 {
  def apply[S, F[_], A](f: S => F[(A, S)]): StateT[S, F, A] = new StateT[S, F, A] {
    def apply(s: S) = f(s)
  }

  implicit def stateTMonadState[S, F[_]](implicit F0: Monad[F]) = new StateTMonadState[S, F] {
    implicit def F: Monad[F] = F0
  }

  trait StateTF[S, G[_]] {
    type f[x] = StateT[S, G, x]
  }

  implicit def StateMonadTrans[S] = new MonadTrans[({type f[g[_], a] = StateT[S, g, a]})#f] {
    def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): StateT[S, G, A] =
      StateT(s => G.map(ga)(a => (a, s)))

    def hoist[M[_], N[_]](f: M ~> N) = new (StateTF[S, M]#f ~> StateTF[S, N]#f) {
      def apply[A](action: StateT[S, M, A]) = StateT[S, N, A](s => f(action(s)))
    }
  }
}

trait StateT[S, F[_], A] {
  def apply(s: S): F[(A, S)]

  def !(s: S)(implicit F: Functor[F]): F[A] =
    F.map(apply(s))(_._1)

  // TODO `!!` is a placeholder.
  def !!(s: S)(implicit F: Functor[F]): F[S] =
    F.map(apply(s))(_._2)
}

object StateT extends StateTs

trait StateTFunctor[S, F[_]] extends Functor[({type f[a] = StateT[S, F, a]})#f] {
  implicit def F: Functor[F]

  override def map[A, B](fa: StateT[S, F, A])(f: A => B): StateT[S, F, B] =
    StateT(s => F.map(fa(s)) {
      case (a, s) => (f(a), s)
    })
}

trait StateTPointed[S, F[_]] extends StateTFunctor[S, F] with Pointed[({type f[a] = StateT[S, F, a]})#f] {
  implicit def F: Pointed[F]

  def pure[A](a: => A): StateT[S, F, A] = StateT(s => F.pure(a, s))
}

trait StateTMonadState[S, F[_]] extends MonadState[({type f[s, a] = StateT[s, F, a]})#f, S] with StateTPointed[S, F] {
  implicit def F: Monad[F]

  def bind[A, B](fa: StateT[S, F, A])(f: A => StateT[S, F, B]): StateT[S, F, B] =
    StateT(s => F.bind(fa(s)) {
      case (a, s) => f(a)(s)
    })

  def init: StateT[S, F, S] = StateT(s => F.pure((s, s)))

  def put(s: S): StateT[S, F, Unit] = StateT(_ => F.pure((s, s)))
}
