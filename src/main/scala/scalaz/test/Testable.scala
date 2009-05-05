package scalaz.test

sealed trait Testable[-A] {
  def test(a: A): Property
}

object Testable {
  def testable[A](f: A => Property) = new Testable[A] {
    def test(a: A) = f(a)
  }

  implicit val TestableCofunctor: Cofunctor[Testable] = new Cofunctor[Testable] {
    def comap[A, B](r: Testable[A], f: B => A) = testable[B](b => r.test(f(b)))
  }
}