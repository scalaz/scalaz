package scalaz

import org.scalacheck.{Gen, Arbitrary}

trait FunctorImplicits {
  implicit val GenFunctor: Functor[Gen] = new Functor[Gen] {
    def fmap[A, B](r: Gen[A], f: A => B) = r map f
  }

  // TODO Make this compile.
  implicit val ArbitraryFunctor: Functor[Arbitrary] = new Functor[Arbitrary] {
    def fmap[A, B](r: Arbitrary[A], f: A => B) = error("make this compile") //r map f
  }
}
