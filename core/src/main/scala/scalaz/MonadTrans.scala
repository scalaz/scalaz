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

  implicit def KleisliMonadTrans[T]: MonadTrans[({type λ[α[_], β] = Kleisli[T, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = Kleisli[T, α, β]})#λ] {
    def lift[G[_] : Monad, A](a: G[A]): Kleisli[T, G, A] =
      Kleisli.kleisli(_ => a)
  }

  implicit val LazyOptionTMonadTrans: MonadTrans[LazyOptionT] = new MonadTrans[LazyOptionT] {
    def lift[G[_] : Monad, A](a: G[A]): LazyOptionT[G, A] =
      LazyOptionT.lazyOptionT(implicitly[Monad[G]].fmap((a: A) => LazyOption.lazySome(a))(a))
  }

}