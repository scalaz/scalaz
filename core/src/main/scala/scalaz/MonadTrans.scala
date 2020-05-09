package scalaz

/** Class of monad transformers. */
trait MonadTrans[F[_[_], _]] {
  /** A component of `Applicative.point` for the transformer stack. */
  def liftM[G[_] : Monad, A](a: G[A]): F[G, A]

  /** A version of `liftM` that infers the type constructor `G`. */
  final def liftMU[GA](a: GA)(implicit G: Unapply[Monad, GA]): F[G.M, G.A] =
    liftM[G.M, G.A](G(a))(G.TC)

  def wrapEffect[G[_]: Monad, A](a: G[F[G, A]]): F[G, A] =
    apply[G].join(liftM(a))

  /** The [[scalaz.Monad]] implied by this transformer. */
  implicit def apply[G[_] : Monad]: Monad[F[G, *]]

  def mapF[G[_], A, B](fa: F[G, A])(f: A => G[B])(implicit M: Monad[G]): F[G, B] =
    Monad[F[G, *]].bind(fa)(a => liftM(f(a)))

  trait MonadTransLaw {
    /** Lifted `point` is `point` of this transformer's monad. */
    def identity[G[_], A](a: A)(implicit G: Monad[G], FA: Equal[F[G, A]]): Boolean =
      FA.equal(liftM(G.point(a)), Monad[F[G, *]].point(a))

    /** `bind` and then `liftM` is the same as `liftM` and then `bind` */
    def composition[G[_], A, B](ga: G[A], f: A => G[B])(implicit G: Monad[G], FB: Equal[F[G, B]]): Boolean =
      FB.equal(liftM(Monad[G].bind(ga)(f)), Monad[F[G, *]].bind(liftM(ga))(a => liftM(f(a))))
  }
  def monadTransLaw = new MonadTransLaw {}
}

object MonadTrans {
  def apply[F[_[_], _]](implicit F: MonadTrans[F]): MonadTrans[F] = F
}

trait Hoist[F[_[_], _]] extends MonadTrans[F] {
  def hoist[M[_]: Monad, N[_]](f: M ~> N): F[M, *] ~> F[N, *]
}

object Hoist {
  def apply[F[_[_], _]](implicit F: Hoist[F]): Hoist[F] = F
}
