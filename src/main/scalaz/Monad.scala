package scalaz

sealed trait Monad[M[_]] {
  implicit val pure: Pure[M]
  implicit val bind: Bind[M]

  implicit val functor = new Functor[M] {
    def fmap[A, B](fa: M[A], f: A => B) = bind.bind(fa, (a: A) => pure.pure(f(a)))
  }

  implicit val pointed = Pointed.pointed[M]

  implicit val apply = new Apply[M] {
    def apply[A, B](f: M[A => B], a: M[A]): M[B] = bind.bind(f, (k: A => B) => functor.fmap(a, k(_: A)))
  }
}

object Monad {
  def monad[M[_]](implicit b: Bind[M], p: Pure[M]) = new Monad[M] {
    val pure = p
    val bind = b
  }

  implicit val IdentityMonad: Monad[Identity] = monad[Identity]

  implicit def ContinuationMonad[R] = monad[PartialApply1Of2[Continuation, R]#Apply]

  implicit val NonEmptyListMonad = monad[NonEmptyList]

  implicit def StateMonad[S] = monad[PartialApply1Of2[State, S]#Apply]

  implicit val Tuple1Monad = monad[Tuple1]

  implicit val Function0Monad = monad[Function0]

  implicit def Function1Monad[R] = monad[PartialApply1Of2[Function1, R]#Apply]

  implicit def Function2Monad[R, S] = monad[PartialApply2Of3[Function2, R, S]#Apply]

  implicit def Function3Monad[R, S, T] = monad[PartialApply3Of4[Function3, R, S, T]#Apply]

  implicit def Function4Monad[R, S, T, U] = monad[PartialApply4Of5[Function4, R, S, T, U]#Apply]

  implicit def Function5Monad[R, S, T, U, V] = monad[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]

  implicit def Function6Monad[R, S, T, U, V, W] = monad[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply]

  implicit val ListMonad = monad[List]

  implicit val StreamMonad = monad[Stream]

  implicit val OptionMonad = monad[Option]

  implicit val ArrayMonad = monad[Array]

  implicit def EitherLeftMonad[X] = monad[PartialApply1Of2[Either.LeftProjection, X]#Flip]

  implicit def EitherRightMonad[X] = monad[PartialApply1Of2[Either.RightProjection, X]#Apply]
}
