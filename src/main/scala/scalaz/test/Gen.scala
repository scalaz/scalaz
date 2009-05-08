package scalaz.test

sealed trait Gen[+A] {
  val f: (Int, Rand) => A

  def apply(sz: Int)(implicit r: Rand) = f(sz, r)
}

object Gen {
  def gen[A](k: (Int, Rand) => A) = new Gen[A] {
    val f = k
  }

  def gens[A](k: Int => A) = gen((sz, _) => k(sz))

  def genr[A](k: Rand => A) = gen((_, r) => k(r))

  import S._

  def fail[T]: Gen[T] = gen((_, _) => error("Failing generator"))

  def parameterised[A](f: (Int, Rand) => Gen[A]) = gen(f >>= (_.f))

  def sized[A](f: Int => Gen[A]): Gen[A] = parameterised((sz, _) => f(sz))

  def randomised[A](f: Rand => Gen[A]): Gen[A] = parameterised((_, r) => f(r))


  implicit val GenFunctor: Functor[Gen] = new Functor[Gen] {
    def fmap[A, B](r: Gen[A], f: A => B) = gen((sz, rd) => f(r(sz)(rd)))
  }

  implicit val GenBind: Bind[Gen] = new Bind[Gen] {
    def bind[A, B](a: Gen[A], f: A => Gen[B]) = gen((sz, rd) =>
      f(a(sz)(rd))(sz)(rd))
  }

  implicit val GenPure: Pure[Gen] = new Pure[Gen] {
    def pure[A](a: => A) = gen((_, _) => a)
  }

  implicit val GenApply: Apply[Gen] = new Apply[Gen] {
    def apply[A, B](f: Gen[A => B], a: Gen[A]) = gen((sz, rd) =>
      f(sz)(rd)(a(sz)(rd)))
  }

  implicit val GenApplicative: Applicative[Gen] = Applicative.applicative[Gen]

  implicit val GenMonad: Monad[Gen] = Monad.monad[Gen]
}