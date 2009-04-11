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

  implicit def OptionApply = FunctorBindApply[Option]
}
