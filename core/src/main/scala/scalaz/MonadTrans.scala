package scalaz


trait MonadTrans[F[_[_], _]] {
  def lift[G[_] : Monad, A](a: G[A]): F[G, A]
}

object MonadTrans extends MonadTranss

trait MonadTranss {
  implicit def EitherTMonadTrans[Z]: MonadTrans[({type λ[α[_], β] = EitherT[Z, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = EitherT[Z, α, β]})#λ] {
    def lift[G[_] : Monad, A](a: G[A]): EitherT[Z, G, A] =
      EitherT.eitherT(implicitly[Monad[G]].fmap((a: A) => Right(a): Either[Z, A])(a))
  }

  implicit val OptionTMonadTrans: MonadTrans[OptionT] = new MonadTrans[OptionT] {
    def lift[G[_] : Monad, A](a: G[A]): OptionT[G, A] =
      OptionT.optionT(implicitly[Monad[G]].fmap((a: A) => Some(a): Option[A])(a))
  }

  implicit def KleisliMonadTrans[T]: MonadTrans[({type λ[α[_], β] = Kleisli[T, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = Kleisli[T, α, β]})#λ] {
    def lift[G[_] : Monad, A](a: G[A]): Kleisli[T, G, A] =
      Kleisli.kleisli(_ => a)
  }

  implicit val LazyOptionTMonadTrans: MonadTrans[LazyOptionT] = new MonadTrans[LazyOptionT] {
    def lift[G[_] : Monad, A](a: G[A]): LazyOptionT[G, A] =
      LazyOptionT.lazyOptionT(implicitly[Monad[G]].fmap((a: A) => LazyOption.lazySome(a))(a))
  }

  implicit def StateTMonadTrans[S]: MonadTrans[({type λ[α[_], β] = StateT[S, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = StateT[S, α, β]})#λ] {
    def lift[G[_] : Monad, A](a: G[A]): StateT[S, G, A] =
      StateT.stateT(s => implicitly[Monad[G]].fmap((a: A) => (a, s))(a))
  }

  implicit val StepListTMonadTrans: MonadTrans[StepListT] = new MonadTrans[StepListT] {
    def lift[G[_] : Monad, A](a: G[A]) =
      StepListT.liftStepListT(a)
  }

  implicit val StepStreamTMonadTrans: MonadTrans[StepStreamT] = new MonadTrans[StepStreamT] {
    def lift[G[_] : Monad, A](a: G[A]) =
      StepStreamT.liftStepStreamT(a)
  }

}