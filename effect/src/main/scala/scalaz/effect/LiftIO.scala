package scalaz
package effect

////
/**
 *
 */
////
trait LiftIO[F[_]]  { self =>
  ////

  def liftIO[A](ioa: IO[A]): F[A]

  // derived functions

  ////
  val liftIOSyntax = new scalaz.syntax.effect.LiftIOSyntax[F] {}
}

object LiftIO {
  @inline def apply[F[_]](implicit F: LiftIO[F]): LiftIO[F] = F

  ////

  implicit def kleisliLiftIO[M[_], R](implicit M0: LiftIO[M]): LiftIO[({type λ[α] = Kleisli[M, R, α]})#λ] = new KleisliLiftIO[M, R] {
    implicit def L = M0
  }
  
  trait KleisliLiftIO[M[_], R] extends LiftIO[({type λ[α] = Kleisli[M, R, α]})#λ] {
    implicit def L: LiftIO[M]
    
    def liftIO[A](ioa: IO[A]) = Kleisli(_ => L.liftIO(ioa))
  }
  
  implicit def StateTLiftIO[M[_], S](implicit M0: MonadIO[M]): LiftIO[({type λ[α] = StateT[M, S, α]})#λ] = new StateTLiftIO[M, S] {
    implicit def M = M0
  }

  trait StateTLiftIO[M[_], S] extends LiftIO[({type λ[α] = StateT[M, S, α]})#λ] {
    implicit def M: MonadIO[M]
    
    def liftIO[A](ioa: IO[A]) = MonadTrans[({type λ[α[_], β] = StateT[α, S, β]})#λ].liftM(M.liftIO(ioa))
  }
  ////
}
