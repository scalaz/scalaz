package scalaz.test

sealed trait Arbitrary[+A] {
  val gen: Gen[A]
}

object Arbitrary {
  def arbitrary[A](g: Gen[A]) = new Arbitrary[A] {
    val gen = g
  }

  import S._

  implicit val ArbitraryFunctor: Functor[Arbitrary] = new Functor[Arbitrary] {
    def fmap[A, B](r: Arbitrary[A], f: A => B) = r.gen |> f arbitrary
  }

  implicit val ArbitraryBind: Bind[Arbitrary] = new Bind[Arbitrary] {
    def bind[A, B](a: Arbitrary[A], f: A => Arbitrary[B]) = a.gen >>= (f(_).gen) arbitrary
  }

  implicit val ArbitraryPure: Pure[Arbitrary] = new Pure[Arbitrary] {
    def pure[A](a: => A) = a.pure[Gen].arbitrary
  }

  implicit val ArbitraryApply: Apply[Arbitrary] = new Apply[Arbitrary] {
    def apply[A, B](f: Arbitrary[A => B], a: Arbitrary[A]) = a.gen <*> f.gen arbitrary
  }

  implicit val ArbitraryApplicative: Applicative[Arbitrary] = Applicative.applicative[Arbitrary]

  implicit val ArbitraryMonad: Monad[Arbitrary] = Monad.monad[Arbitrary]
}
