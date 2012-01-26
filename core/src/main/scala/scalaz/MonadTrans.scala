package scalaz

trait MonadTrans[F[_[_], _]] {
  def liftM[G[_] : Monad, A](a: G[A]): F[G, A]

  implicit def apply[G[_] : Monad]: Monad[({type λ[α] = F[G, α]})#λ]
}

object MonadTrans {
  def apply[F[_[_], _]](implicit F: MonadTrans[F]): MonadTrans[F] = F
}

trait Hoist[F[_[_], _]] extends MonadTrans[F] {
  def hoist[M[_]: Monad, N[_]](f: M ~> N): ({type f[x] = F[M, x]})#f ~> ({type f[x] = F[N, x]})#f
}

/**
 * This trait establishes a partial order among monads. A "bigger" monad
 * is one that does all of the effects of the "smaller" as part of its 
 * execution.
 */
trait MonadPartialOrder[M1[_], M2[_]] {
  implicit def M1M: Monad[M1]
  implicit def M2M: Monad[M2]

  def promote[A](m2: M2[A]): M1[A]
}

trait MonadPartialOrderFunctions {
  // the identity ordering
  implicit def identity[M[_]: Monad]: MonadPartialOrder[M, M] = 
    new MonadPartialOrder[M, M] {
      val M1M = Monad[M]
      val M2M = Monad[M]
      def promote[A](m: M[A]) = m
    }

  implicit def transformer[M2[_]: Monad, F[_[_], _]: MonadTrans]: MonadPartialOrder[({ type λ[α] = F[M2, α] })#λ, M2] = 
    new MonadPartialOrder[({ type λ[α] = F[M2, α] })#λ, M2] {
      val M1M = MonadTrans[F].apply[M2]
      val M2M = Monad[M2]
      def promote[A](m2: M2[A]) = MonadTrans[F].liftM(m2)
    }

  // can't be implicit, or else implicit search doesn't terminate
  def transitive[M1[_], M2[_], M3[_]](implicit e1: MonadPartialOrder[M1, M2], e2: MonadPartialOrder[M2, M3]): MonadPartialOrder[M1, M3] = 
    new MonadPartialOrder[M1, M3] {
      val M1M = e1.M1M
      val M2M = e2.M2M
      def promote[A](m3: M3[A]) = e1.promote(e2.promote(m3))
    }
}

object MonadPartialOrder extends MonadPartialOrderFunctions
