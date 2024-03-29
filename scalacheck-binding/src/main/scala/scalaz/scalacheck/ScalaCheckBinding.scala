package scalaz
package scalacheck

/**
 * Type class instances for types from [[https://github.com/typelevel/scalacheck Scalacheck]]
 */
object ScalaCheckBinding {
  import org.scalacheck.{Cogen, Gen, Arbitrary, Shrink}
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

  implicit val CogenInstance: Divisible[Cogen] = new Divisible[Cogen] {
    override def contramap[A, B](a: Cogen[A])(f: B => A) =
      a contramap f
    override def conquer[A] =
      Cogen((seed, _) => seed)
    override def divide2[A, B, C](fa: =>Cogen[A], fb: =>Cogen[B])(f: C => (A, B)) =
      Cogen{ (seed, c) =>
        val (a, b) = f(c)
        fb.perturb(fa.perturb(seed, a), b)
      }
  }

  implicit val ShrinkFunctor: InvariantFunctor[Shrink] =
    new InvariantFunctor[Shrink] {
      def xmap[A, B](ma: Shrink[A], f: A => B, g: B => A): Shrink[B] =
        Shrink{b => ma shrink g(b) map f}
    }
}

// vim: expandtab:ts=2:sw=2
