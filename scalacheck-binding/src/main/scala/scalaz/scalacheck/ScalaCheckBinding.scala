package scalaz
package scalacheck

/**
 * Type class instances for types from [[https://github.com/rickynils/scalacheck Scalacheck]]
 */
object ScalaCheckBinding extends ScalaCheckBinding1 {
  import org.scalacheck.{Gen, Arbitrary, Shrink}
  import Gen.{sized, const}

  implicit val ArbitraryMonad: Monad[Arbitrary] = new Monad[Arbitrary] {
    def bind[A, B](fa: Arbitrary[A])(f: A => Arbitrary[B]) = Arbitrary(fa.arbitrary.flatMap(f(_).arbitrary))
    def point[A](a: => A) = Arbitrary(sized(_ => const(a)))
    override def map[A, B](fa: Arbitrary[A])(f: A => B) = Arbitrary(fa.arbitrary.map(f))
  }

  implicit val GenMonad: Monad[Gen] = new Monad[Gen] {
    def point[A](a: => A) = sized(_ => const(a))
    def bind[A, B](fa: Gen[A])(f: A => Gen[B]) = fa flatMap f
    override def map[A, B](fa: Gen[A])(f: A => B) = fa map f
  }

  implicit val ShrinkFunctor: InvariantFunctor[Shrink] =
    new InvariantFunctor[Shrink] {
      def xmap[A, B](ma: Shrink[A], f: A => B, g: B => A): Shrink[B] =
        Shrink{b => ma shrink g(b) map f}
    }
}

// vim: expandtab:ts=2:sw=2
