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
sealed trait MonadPartialOrder[M1[_], M2[_]] extends NaturalTransformation[M2, M1] { self =>
  implicit def M1M: Monad[M1]
  implicit def M2M: Monad[M2]

  def apply[A](m2: M2[A]) = promote(m2)
  def promote[A](m2: M2[A]): M1[A]

  def compose[M[_]](mo: MonadPartialOrder[M, M1]): MonadPartialOrder[M, M2] = 
    new MonadPartialOrder[M, M2] {
      val M1M = mo.M1M
      val M2M = self.M2M
      def promote[A](m2: M2[A]) = mo.promote(self.promote(m2))
    }

  def transform[F[_[_], _]: MonadTrans]: MonadPartialOrder[({ type λ[α] = F[M1, α] })#λ, M2] = 
    new MonadPartialOrder[({ type λ[α] = F[M1, α] })#λ, M2] {
      val M1M = MonadTrans[F].apply[M1](self.M1M)
      val M2M = self.M2M
      def promote[A](m2: M2[A]) = MonadTrans[F].liftM(self.promote(m2))(self.M1M)
    }
}

trait MonadPartialOrderFunctions1 {
  implicit def transitive[M1[_], M2[_], M3[_]](implicit e1: MonadPartialOrder[M1, M2], e2: MonadPartialOrder[M2, M3]): MonadPartialOrder[M1, M3] = 
     e2 compose e1
}

trait MonadPartialOrderFunctions extends MonadPartialOrderFunctions1 {
  // the identity ordering
  implicit def identity[M[_]: Monad]: MonadPartialOrder[M, M] = 
    new MonadPartialOrder[M, M] {
      val M1M = Monad[M]
      val M2M = Monad[M]
      def promote[A](m: M[A]) = m
    }

  implicit def transformer[M[_]: Monad, F[_[_], _]: MonadTrans]: MonadPartialOrder[({ type λ[α] = F[M, α] })#λ, M] = 
    identity[M].transform[F]
}

object MonadPartialOrder extends MonadPartialOrderFunctions
