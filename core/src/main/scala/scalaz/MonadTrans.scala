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
  implicit def apply[G[_] : Monad]: Monad[F[G, ?]]

  trait MonadTransLaw {
    // For each monad `G[_]`, `liftM` must be injective monad homomorphism from `G` to `F[G, ?]`.

    /** Lifted `point` is `point` of this transformer's monad. */
    def pointPreserving[G[_], A](a: A)(implicit G: Monad[G], FA: Equal[F[G, A]]): Boolean =
      FA.equal(liftM(G.point(a)), Monad[F[G, ?]].point(a))

    /** `bind` and then `liftM` is the same as `liftM` and then `bind` */
    def bindPreserving[G[_], A, B](ga: G[A], f: A => G[B])(implicit G: Monad[G], FB: Equal[F[G, B]]): Boolean =
      FB.equal(liftM(Monad[G].bind(ga)(f)), Monad[F[G, ?]].bind(liftM(ga))(a => liftM(f(a))))

    def injective[G[_], A](g1: G[A], g2: G[A])(implicit G: Monad[G], GA: Equal[G[A]], FA: Equal[F[G, A]]): Boolean =
      if(!GA.equal(g1, g2)) !FA.equal(liftM(g1), liftM(g2)) else true
  }
  def monadTransLaw = new MonadTransLaw {}
}

object MonadTrans {
  def apply[F[_[_], _]](implicit F: MonadTrans[F]): MonadTrans[F] = F
}

trait Hoist[F[_[_], _]] extends MonadTrans[F] {
  def hoist[M[_]: Monad, N[_]](f: M ~> N): F[M, ?] ~> F[N, ?]
}

object Hoist {
  def apply[F[_[_], _]](implicit F: Hoist[F]): Hoist[F] = F
}

/**
 * This trait establishes a partial order among monads. A "bigger" monad
 * is one that does all of the effects of the "smaller" as part of its
 * execution.
 */
trait MonadPartialOrder[G[_], F[_]] extends NaturalTransformation[F, G] { self =>
  implicit val MG: Monad[G]
  implicit val MF: Monad[F]

  def apply[A](m2: F[A]) = promote(m2)
  def promote[A](m2: F[A]): G[A]

  def compose[M[_]](mo: MonadPartialOrder[M, G]): MonadPartialOrder[M, F] =
    new MonadPartialOrder[M, F] {
      val MG = mo.MG
      val MF = self.MF
      def promote[A](m2: F[A]) = mo.promote(self.promote(m2))
    }

  def transform[T[_[_], _]: MonadTrans]: MonadPartialOrder[T[G, ?], F] =
    compose[T[G, ?]](MonadPartialOrder.transformer[G, T])

  trait MonadPartialOrderLaw {
    // MonadPartialOrder is required to be injective monad homomorphism

    def pointPreserving[A](a: A)(implicit GA: Equal[G[A]]): Boolean =
      GA.equal(promote(MF.point(a)), MG.point(a))

    def bindPreserving[A, B](fa: F[A], f: A => F[B])(implicit GB: Equal[G[B]]): Boolean =
      GB.equal(promote(MF.bind(fa)(f)), MG.bind(promote(fa))(a => promote(f(a))))

    def injective[A](f1: F[A], f2: F[A])(implicit FA: Equal[F[A]], GA: Equal[G[A]]): Boolean =
      if(!FA.equal(f1, f2)) !GA.equal(promote(f1), promote(f2)) else true
  }
  def monadPartialOrderLaw = new MonadPartialOrderLaw {}
}

sealed abstract class MonadPartialOrderFunctions1 {
  implicit def transitive[G[_], F[_], E[_]](implicit e1: MonadPartialOrder[G, F], e2: MonadPartialOrder[F, E]): MonadPartialOrder[G, E] =
     e2 compose e1
}

sealed abstract class MonadPartialOrderFunctions extends MonadPartialOrderFunctions1 {
  // the identity ordering
  implicit def id[M[_]: Monad]: MonadPartialOrder[M, M] =
    new MonadPartialOrder[M, M] {
      val MG = Monad[M]
      val MF = Monad[M]
      def promote[A](m: M[A]) = m
    }

  implicit def transformer[M[_], F[_[_], _]](implicit M: Monad[M], F: MonadTrans[F]): MonadPartialOrder[F[M, ?], M] =
    new MonadPartialOrder[F[M, ?], M] {
      val MG = F[M]
      val MF = M
      def promote[A](m: M[A]) = F.liftM(m)
    }
}

object MonadPartialOrder extends MonadPartialOrderFunctions
