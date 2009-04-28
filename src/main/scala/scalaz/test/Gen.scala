package scalaz.test

sealed trait Gen[+A] {
  def apply(sz: Int)(implicit r: Rand): Option[A]
}

object Gen {
  def gen[A](f: (Int, Rand) => Option[A]) = new Gen[A] {
    def apply(sz: Int)(implicit r: Rand) = f(sz, r)
  }

  implicit val GenFunctor: Functor[Gen] = new Functor[Gen] {
    def fmap[A, B](r: Gen[A], f: A => B) = gen((sz, rd) => r(sz)(rd) map f)    
  }
}
