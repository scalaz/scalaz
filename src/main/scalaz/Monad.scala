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

  implicit val IdentityMonad = monad[Identity]

  implicit def ContinuationMonad[R] = monad[PartialApply1Of2[Continuation, R]#Apply]

  implicit def OptionMonad = monad[Option]
}
