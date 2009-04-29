package scalaz.test

sealed trait Gen[+A] {
  def apply(sz: Int)(implicit r: Rand): Option[A]

  val f = (sz: Int, r: Rand) => apply(sz)(r)

  def filter(f: A => Boolean): Gen[A] = Gen.gen((sz, rd) => for(p <- this(sz)(rd);
                                                                q <- if(f(p)) Some(p) else None) yield q)

  def resize(sz: Int) = Gen.gen((_, r) => apply(sz)(r))
}

object Gen {
  def gen[A](f: (Int, Rand) => Option[A]) = new Gen[A] {
    def apply(sz: Int)(implicit r: Rand) = f(sz, r)
  }

  def gens[A](f: Int => Option[A]) = gen((sz, _) => f(sz))

  def genr[A](f: Rand => Option[A]) = gen((_, r) => f(r))

  import S._

  def fail[T]: Gen[T] = gen((_, _) => None)

  def parameterised[A](f: (Int, Rand) => Gen[A]) = gen(f >>= (_.f))

  def sized[A](f: Int => Gen[A]): Gen[A] = parameterised((sz, _) => f(sz))

  def randomised[A](f: Rand => Gen[A]): Gen[A] = parameterised((_, r) => f(r))

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
