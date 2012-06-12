package scalaz
package effect

object stateTEffect extends StateTEffectInstances

trait StateTEffectInstances0 extends StateTInstances {
  implicit def StateTLiftIO[M[+_], S](implicit M0: MonadIO[M]): LiftIO[({type λ[+α] = StateT[M, S, α]})#λ] = new StateTLiftIO[M, S] {
    implicit def M = M0
  }
}

trait StateTEffectInstances extends StateTEffectInstances0 {
  implicit def StateTMonadIO[M[+_], S](implicit M0: MonadIO[M]): MonadIO[({type λ[+α] = StateT[M, S, α]})#λ] = {
    new MonadIO[({type λ[+α] = StateT[M, S, α]})#λ] with StateTLiftIO[M, S] with StateTMonadState[S, M] {
      implicit def F = M0
      implicit def M = M0
    }
  }  
}

trait StateTLiftIO[M[+_], S] extends LiftIO[({type λ[α] = StateT[M, S, α]})#λ] {
  implicit def M: MonadIO[M]
    
  def liftIO[A](ioa: IO[A]) = MonadTrans[({type λ[α[+_], +β] = StateT[α, S, β]})#λ].liftM(M.liftIO(ioa))
}
