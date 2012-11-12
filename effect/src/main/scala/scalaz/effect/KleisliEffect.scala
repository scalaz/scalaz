package scalaz
package effect

object kleisliEffect extends KleisliEffectInstances  

trait KleisliEffectInstances1 extends KleisliInstances {
  implicit def kleisliLiftIO[M[+_], R](implicit M0: LiftIO[M]): LiftIO[({type λ[α] = Kleisli[M, R, α]})#λ] = new KleisliLiftIO[M, R] {
    implicit def L = M0
  }
}

trait KleisliEffectInstances0 extends KleisliEffectInstances1 {
  implicit def KleisliMonadIO[R, M[+_]](implicit M0: MonadIO[M]): MonadIO[({type λ[α] = Kleisli[M, R, α]})#λ] =
    new MonadIO[({type λ[α] = Kleisli[M, R, α]})#λ] with KleisliLiftIO[M, R] with KleisliMonadReader[M, R] {
      implicit def L = M0
      implicit def F = M0
    }
}

trait KleisliEffectInstances extends KleisliEffectInstances0 {
  implicit def KleisliMonadCatchIO[M[+_], R](implicit M0: MonadCatchIO[M]): MonadCatchIO[({type λ[α] = Kleisli[M, R, α]})#λ] =
    new KleisliCatchIO[M, R] {
      implicit def L = M0
      implicit def F = M0
    }
}

trait KleisliLiftIO[M[+_], R] extends LiftIO[({type λ[α] = Kleisli[M, R, α]})#λ] {
  implicit def L: LiftIO[M]
    
  def liftIO[A](ioa: IO[A]) = Kleisli(_ => L.liftIO(ioa))
}

trait KleisliCatchIO[M[+_], R] extends MonadCatchIO[({type λ[α] = Kleisli[M, R, α]})#λ] with KleisliLiftIO[M, R] with KleisliMonadReader[M, R] {
  implicit def F: MonadCatchIO[M]

  def except[A](k: Kleisli[M, R, A])(h: Throwable ⇒ Kleisli[M, R, A]) = 
    Kleisli(r ⇒ F.except(k.run(r))(t ⇒ h(t).run(r)))
}
