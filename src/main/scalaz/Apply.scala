package scalaz

trait Apply[Z[_]] {
  def apply[A, B](f: Z[A => B], a: Z[A]): Z[B]
}

object Apply {
  def FunctorBindApply[Z[_]](implicit t: Functor[Z], b: Bind[Z]) = new Apply[Z] {
    def apply[A, B](f: Z[A => B], a: Z[A]): Z[B] = {
      b.bind(f, (g: A => B) => t.fmap(a, g(_: A)))
    }
  }

  implicit val IdentityApply: Apply[Identity] = FunctorBindApply[Identity]

  implicit def ContinuationApply[R] = FunctorBindApply[PartialApply1Of2[Continuation, R]#Apply]

  implicit val NonEmptyListApply = FunctorBindApply[NonEmptyList]

  implicit def StateApply[S] = FunctorBindApply[PartialApply1Of2[State, S]#Apply]

  implicit val Tuple1Apply = FunctorBindApply[Tuple1]

  implicit val Function0Apply = FunctorBindApply[Function0]

  implicit def Function1Apply[R] = FunctorBindApply[PartialApply1Of2[Function1, R]#Apply]

  implicit def Function2Apply[R, S] = FunctorBindApply[PartialApply2Of3[Function2, R, S]#Apply]

  implicit def Function3Apply[R, S, T] = FunctorBindApply[PartialApply3Of4[Function3, R, S, T]#Apply]

  implicit def Function4Apply[R, S, T, U] = FunctorBindApply[PartialApply4Of5[Function4, R, S, T, U]#Apply]

  implicit def Function5Apply[R, S, T, U, V] = FunctorBindApply[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]

  implicit def Function6Apply[R, S, T, U, V, W] = FunctorBindApply[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply]

  implicit val ListApply = FunctorBindApply[List]

  implicit val StreamApply = FunctorBindApply[Stream]

  implicit val OptionApply = FunctorBindApply[Option]

  implicit val ArrayApply = FunctorBindApply[Array]

  implicit def EitherLeftApply[X] = FunctorBindApply[PartialApply1Of2[Either.LeftProjection, X]#Flip]

  implicit def EitherRightApply[X] = FunctorBindApply[PartialApply1Of2[Either.RightProjection, X]#Apply]

}
