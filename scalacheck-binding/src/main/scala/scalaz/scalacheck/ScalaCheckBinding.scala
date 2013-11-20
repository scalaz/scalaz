package scalaz
package scalacheck

/**
 * Type class instances for types from [[https://github.com/rickynils/scalacheck Scalacheck]]
 */
object ScalaCheckBinding {
  import org.scalacheck.{Gen, Arbitrary}
  import Gen.{sized, value}

  implicit val ArbitraryMonad: Monad[Arbitrary] = new Monad[Arbitrary] {
    def bind[A, B](fa: Arbitrary[A])(f: A => Arbitrary[B]) = Arbitrary(fa.arbitrary.flatMap(f(_).arbitrary))
    def point[A](a: => A) = Arbitrary(sized(_ => value(a)))
    override def map[A, B](fa: Arbitrary[A])(f: A => B) = Arbitrary(fa.arbitrary.map(f))
  }

  implicit val GenMonad: Monad[Gen] = new Monad[Gen] {
    def point[A](a: => A) = sized(_ => value(a))
    def bind[A, B](fa: Gen[A])(f: A => Gen[B]) = fa flatMap f
    override def map[A, B](fa: Gen[A])(f: A => B) = fa map f
  }
}

// vim: expandtab:ts=2:sw=2
