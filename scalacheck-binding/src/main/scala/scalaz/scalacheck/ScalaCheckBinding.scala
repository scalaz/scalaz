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
    override def map[A, B](fa: Gen[A])(f: (A) => B) = fa map f
    override def map2[A, B, C](fa: Gen[A], fb: Gen[B])(f: (A, B) => C) = fa.map2(fb)(f)
    override def map3[A, B, C, D](fa: Gen[A], fb: Gen[B], fc: Gen[C])(f: (A, B, C) => D) = fa.map3(fb, fc)(f)
    override def map4[A, B, C, D, E](fa: Gen[A], fb: Gen[B], fc: Gen[C], fd: Gen[D])(f: (A, B, C, D) => E) = fa.map4(fb, fc, fd)(f)
    override def map5[A, B, C, D, E, R](fa: Gen[A], fb: Gen[B], fc: Gen[C], fd: Gen[D], fe: Gen[E])(f: (A, B, C, D, E) => R) = fa.map5(fb, fc, fd, fe)(f)
    override def ap[A, B](fa: Gen[A])(f: Gen[(A) => B]) = fa ap f
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
