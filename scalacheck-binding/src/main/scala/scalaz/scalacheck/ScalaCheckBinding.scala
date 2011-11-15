package scalaz
package scalacheck

/**
 * Type class instances for types from <a href="http://code.google.com/p/scalacheck">Scalacheck</a>
 */
object ScalaCheckBinding {
  import Scalaz._
  import org.scalacheck.{Gen, Arbitrary}
  import Gen.{sized, value}

  implicit val ArbitraryMonad: Monad[Arbitrary] = new Monad[Arbitrary] {
    def bind[A, B](fa: Arbitrary[A])(f: (A) => Arbitrary[B]) = Arbitrary(fa.arbitrary.flatMap(f(_).arbitrary))

    def point[A](a: => A) = Arbitrary(sized(_ => value(a)))
  }

  implicit val GenMonad: Monad[Gen] = new Monad[Gen] {
    def bind[A, B](fa: Gen[A])(f: (A) => Gen[B]) = fa flatMap f
    def point[A](a: => A) = sized(_ => value(a))
  }

  implicit def GenApply: Apply[Gen] = new Apply[Gen] {
    def map[A, B](fa: Gen[A])(f: (A) => B) = fa.map(f)
    def ap[A, B](fa: Gen[A])(f: Gen[(A) => B]) = fa.ap(f)
  }

  implicit def ArbitraryApply: Apply[Arbitrary] = new Apply[Arbitrary] {
    def map[A, B](fa: Arbitrary[A])(f: (A) => B) = Arbitrary(fa.arbitrary.map(f))
    def ap[A, B](fa: Arbitrary[A])(f: Arbitrary[(A) => B]) = Arbitrary(fa.arbitrary.ap(f.arbitrary))
  }
}
