package scalaz
package scalacheck

/**
 * Type class instances for types from <a href="http://code.google.com/p/scalacheck">Scalacheck</a>
 */
object ScalaCheckBinding {
  import org.scalacheck.{Gen, Arbitrary}
  import Gen.{sized, value}
  import typelevel._

  implicit val ArbitraryMonad: Monad[Arbitrary] = new Monad[Arbitrary] {
    def bind[A, B](fa: Arbitrary[A])(f: (A) => Arbitrary[B]) = Arbitrary(fa.arbitrary.flatMap(f(_).arbitrary))
    def point[A](a: => A) = Arbitrary(sized(_ => value(a)))
    override def map[A, B](fa: Arbitrary[A])(f: (A) => B) = Arbitrary(fa.arbitrary.map(f))
    override def ap[A, B](fa: => Arbitrary[A])(f: => Arbitrary[(A) => B]) = Arbitrary(fa.arbitrary.ap(f.arbitrary))
  }

  implicit val GenMonad: Monad[Gen] = new Monad[Gen] {
    def point[A](a: => A) = sized(_ => value(a))
    override def ap[A, B](fa: => Gen[A])(f: => Gen[(A) => B]) = fa ap f
    def bind[A, B](fa: Gen[A])(f: (A) => Gen[B]) = fa flatMap f
    override def map[A, B](fa: Gen[A])(f: (A) => B) = fa map f
    override def apply[A, B, C](fa: => Gen[A], fb: => Gen[B])(f: (A, B) => C) = fa.map2(fb)(f)
    override def apply[A, B, C, D](fa: => Gen[A], fb: => Gen[B], fc: => Gen[C])(f: (A, B, C) => D) = fa.map3(fb, fc)(f)
    override def apply[A, B, C, D, E](fa: => Gen[A], fb: => Gen[B], fc: => Gen[C], fd: => Gen[D])(f: (A, B, C, D) => E) = fa.map4(fb, fc, fd)(f)
    override def apply[A, B, C, D, E, R](fa: => Gen[A], fb: => Gen[B], fc: => Gen[C], fd: => Gen[D], fe: => Gen[E])(f: (A, B, C, D, E) => R) = fa.map5(fb, fc, fd, fe)(f)
  }

  implicit val ArbitraryClass: TypeClass[Arbitrary] = new TypeClass[Arbitrary] {
    import typelevel.syntax.hlist._

    def emptyProduct = Arbitrary(value(HNil))

    def product[F, T <: HList](FHead: Arbitrary[F], FTail: Arbitrary[T]): Arbitrary[F :: T] =
      Applicative[Arbitrary].apply(FHead, FTail)(_ :: _)
  }

}

// vim: expandtab:ts=2:sw=2
