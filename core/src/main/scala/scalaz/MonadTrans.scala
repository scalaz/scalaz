package scalaz

/** Class of monad transformers. */
trait MonadTrans[F[_[_], _]] {
  /** A component of `Applicative.point` for the transformer stack. */
  def liftM[G[_] : Monad, A](a: G[A]): F[G, A]

  /** A version of `liftM` that infers the type constructor `G`. */
  final def liftMU[GA](a: GA)(implicit G: Unapply[Monad, GA]): F[G.M, G.A] =
    liftM[G.M, G.A](G(a))(G.TC)

  /** The [[scalaz.Monad]] implied by this transformer. */
  implicit def apply[G[_] : Monad]: Monad[F[G, ?]]
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
    new MonadPartialOrder[T[G, ?], F] {
      val MG = MonadTrans[T].apply[G](self.MG)
      val MF = self.MF
      def promote[A](m2: F[A]) = MonadTrans[T].liftM(self.promote(m2))(self.MG)
    }
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

  implicit def transformer[M[_]: Monad, F[_[_], _]: MonadTrans]: MonadPartialOrder[F[M, ?], M] =
    id[M].transform[F]
}

object MonadPartialOrder extends MonadPartialOrderFunctions
