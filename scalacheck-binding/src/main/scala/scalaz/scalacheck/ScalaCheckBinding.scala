package scalaz
package scalacheck

/**
 * Type class instances for types from <a href="http://code.google.com/p/scalacheck">Scalacheck</a>
 */
object ScalaCheckBinding {
  import Scalaz._
  import org.scalacheck.{Gen, Arbitrary}
  import Gen.{sized, value}

  implicit val GenBind: Bind[Gen] = new Bind[Gen] {
    def bind[A, B](fa: Gen[A])(f: A => Gen[B]) = fa flatMap f
    def map[A, B](fa: Gen[A])(f: (A) => B) = fa map f
  }

  implicit val GenFunctor: Functor[Gen] = new Functor[Gen] {
    def map[A, B](fa: Gen[A])(f: (A) => B) = fa map f
  }

  implicit val ArbitraryFunctor: Functor[Arbitrary] = new Functor[Arbitrary] {
    def map[A, B](fa: Arbitrary[A])(f: A => B) = Arbitrary(fa.arbitrary map f)
  }

  implicit val GenPure: Pointed[Gen] = new Pointed[Gen] {
    def point[A](a: => A): Gen[A] = sized(_ => value(a))
    def map[A, B](fa: Gen[A])(f: (A) => B) = fa.map(f)
  }

  implicit val ArbitraryMonad: Monad[Arbitrary] = new Monad[Arbitrary] {
    def bind[A, B](fa: Arbitrary[A])(f: (A) => Arbitrary[B]) = Arbitrary(fa.arbitrary.flatMap(f(_).arbitrary))

    def point[A](a: => A) = Arbitrary(sized(_ => value(a)))
  }

  implicit def GenSemigroup[A](implicit s: Semigroup[A]) = new Semigroup[Gen[A]]{
    def append(f1: Gen[A], f2: => Gen[A]) = Gen(p => f1(p) |+| f2(p))
  }

  implicit def ArbitrarySemigroup[A](implicit s: Semigroup[A]) = new Semigroup[Arbitrary[A]] {
    def append(f1: Arbitrary[A], f2: => Arbitrary[A]) = Arbitrary(f1.arbitrary |+| f2.arbitrary)
  }

  implicit def GenApply: Apply[Gen] = new Apply[Gen] {
    def map[A, B](fa: Gen[A])(f: (A) => B) = fa.map(f)
    def ap[A, B](fa: Gen[A])(f: Gen[(A) => B]) = fa.ap(f)
  }

  implicit def ArbitraryApply: Apply[Arbitrary] = new Apply[Arbitrary] {
    def map[A, B](fa: Arbitrary[A])(f: (A) => B) = Arbitrary(fa.arbitrary.map(f))
    def ap[A, B](fa: Arbitrary[A])(f: Arbitrary[(A) => B]) = Arbitrary(fa.arbitrary.ap(f.arbitrary))
  }

  implicit def GenZero[A](implicit m: Monoid[A]) = Gen(_ => Some(m.zero))

  implicit def ArbitraryZero[A] = Arbitrary(Gen(_ => None))
}
