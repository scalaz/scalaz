package scalaz.test

sealed trait Gen[+A] {
  def apply(sz: Int)(implicit r: Rand): Option[A]
}

object Gen {
  def gen[A](f: (Int, Rand) => Option[A]) = new Gen[A] {
    def apply(sz: Int)(implicit r: Rand) = f(sz, r)
  }

  implicit val GenFunctor: Functor[Gen] = new Functor[Gen] {
    def fmap[A, B](r: Gen[A], f: A => B) = gen((sz, rd) => r(sz)(rd) map f)    
  }

  implicit val GenBind: Bind[Gen] = new Bind[Gen] {
    def bind[A, B](a: Gen[A], f: A => Gen[B]) = gen((sz, rd) =>
      for(p <- a(sz)(rd);
          q <- f(p)(sz)(rd)) yield q)
  }

  implicit val GenPure: Pure[Gen] = new Pure[Gen] {
    def pure[A](a: => A) = gen((_, _) => Some(a))
  }

  implicit val GenApply: Apply[Gen] = new Apply[Gen] {
    def apply[A, B](f: Gen[A => B], a: Gen[A]) = gen((sz, rd) =>
      for(p <- f(sz)(rd);
          q <- a(sz)(rd)) yield p(q))
  }

  implicit val GenApplicative: Applicative[Gen] = Applicative.applicative[Gen]

  implicit val GenMonad: Monad[Gen] = Monad.monad[Gen]
}
