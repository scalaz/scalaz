package scalaz.test

sealed trait Testable[-A] {
  def test(a: A): Property
}

object Testable {
  def testable[A](f: A => Property) = new Testable[A] {
    def test(a: A) = f(a)
  }

  def testableResult[A](f: A => Result) = testable[A](a => Property.property((_, _) => f(a)))

  import S._

  implicit val TestableCofunctor: Cofunctor[Testable] = new Cofunctor[Testable] {
    def comap[A, B](r: Testable[A], f: B => A) = testable[B](b => r.test(f(b)))
  }

  implicit val UnitTestable: Testable[Unit] = testableResult(_ => Result.undecided)

  implicit val BooleanTestable: Testable[Boolean] = testableResult(b => Result.result(if(b) Status.proven else Status.falsified))

  implicit val ResultTestable: Testable[Result] = testableResult(r => r)

  implicit val PropertyTestable: Testable[Property] = testable(p => p)

  implicit def GenTestable[A](implicit t: Testable[A]): Testable[Gen[A]] = testable(g => Property.property((sz, r) => g.flatMap((a: A) => t.test(a).gen).apply(sz)(r) getOrElse Result.undecided))
}
