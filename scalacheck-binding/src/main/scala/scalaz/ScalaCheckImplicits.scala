package scalaz


object ScalaCheckImplicits {
  import Scalaz._
  import org.scalacheck.{Gen, Arbitrary}
  import Gen.{sized, value}

  implicit val GenBind: Bind[Gen] = new Bind[Gen] {
    def bind[A, B](r: Gen[A], f: A => Gen[B]) = r flatMap f
  }

  implicit val ArbitraryBind: Bind[Arbitrary] = new Bind[Arbitrary] {
    def bind[A, B](r: Arbitrary[A], f: A => Arbitrary[B]) = Arbitrary(r.arbitrary flatMap (f(_).arbitrary))
  }

  implicit val GenFunctor: Functor[Gen] = new Functor[Gen] {
    def fmap[A, B](r: Gen[A], f: A => B) = r map f
  }

  implicit val ArbitraryFunctor: Functor[Arbitrary] = new Functor[Arbitrary] {
    def fmap[A, B](r: Arbitrary[A], f: A => B) = Arbitrary(r.arbitrary map f)
  }

  implicit val GenPure: Pure[Gen] = new Pure[Gen] {
    def pure[A](a: => A): Gen[A] = sized(_ => value(a))
  }

  implicit val ArbitraryPure: Pure[Arbitrary] = new Pure[Arbitrary] {
    def pure[A](a: => A) = Arbitrary(sized(_ => value(a)))
  }

  implicit def GenSemigroup[A](implicit s: Semigroup[A]) = semigroup[Gen[A]]((a, b) => Gen(p => a(p) ⊹ (b apply p)))

  implicit def ArbitrarySemigroup[A](implicit s: Semigroup[A]) = semigroup[Arbitrary[A]]((a, b) => Arbitrary(a.arbitrary ⊹ b.arbitrary))

  implicit def GenZero[A](implicit z: Zero[A]) = Gen(_ => Some(z.zero))

  implicit def ArbitraryZero[A] = Arbitrary(Gen(_ => None))

}