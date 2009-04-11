package scalaz

sealed trait Applicative[Z[_]] {
  implicit val pure: Pure[Z]
  implicit val apply: Apply[Z]

  implicit val functor: Functor[Z] = new Functor[Z] {
    def fmap[A, B](fa: Z[A], f: A => B) = apply(pure.pure(f), fa)
  }

  implicit val pointed = Pointed.pointed[Z]
}

object Applicative {
  def applicative[Z[_]](implicit p: Pure[Z], a: Apply[Z]) = new Applicative[Z] {
    val pure = p
    val apply = a
  }

  implicit val IdentityApplicative = applicative[Identity]

  implicit def ContinuationApplicative[R] = applicative[PartialApply1Of2[Continuation, R]#Apply]

  implicit def OptionApplicative = applicative[Option]
}
