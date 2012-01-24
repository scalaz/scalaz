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
  implicit def identity[M1[_]: Monad]: MonadPartialOrder[M1, M1] = 
    new MonadPartialOrder[M1, M1] {
      val M1M = Monad[M1]
      val M2M = Monad[M1]
      def promote[A](m2: M1[A]) = m2
    }

  implicit def transformer[M1[_]: Monad, F[_[_], _]: MonadTrans]: MonadPartialOrder[({ type λ[α] = F[M1, α] })#λ, M1] = 
    new MonadPartialOrder[({ type λ[α] = F[M1, α] })#λ, M1] {
      val M1M = MonadTrans[F].apply[M1]
      val M2M = Monad[M1]
      def promote[A](m2: M1[A]) = MonadTrans[F].liftM(m2)
    }
}

object MonadPartialOrder extends MonadPartialOrderFunctions
