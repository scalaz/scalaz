package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

object ArrowTest extends SpecLite {

  case class Fn1[A, B](run: A => B)

  implicit def ArbitraryFn1[A, B](implicit A: Arbitrary[A => B]): Arbitrary[Fn1[A, B]] =
    Functor[Arbitrary].map(A)(Fn1(_))

  val isoFn1Function1 = new Isomorphism.IsoBifunctorTemplate[Fn1, Function1] {
    def to[A, B](fn: Fn1[A, B]): (A => B) = fn.run
    def from[A, B](fn: (A => B)): Fn1[A, B] = Fn1(fn)
  }
  implicit val fn1Arrow: Arrow[Fn1] = Arrow.fromIso(isoFn1Function1)
  implicit val fn1Choice: Choice[Fn1] = Choice.fromIso(isoFn1Function1)

  implicit val eqFn1BooleanInt = new Equal[Fn1[Boolean, Int]] {
    def equal(f1: Fn1[Boolean, Int], f2: Fn1[Boolean, Int]): Boolean =
      f1.run(true) == f2.run(true) && f1.run(false) == f2.run(false)
  }

  implicit val ApplyFn1 = {
    import Arrow._
    // obtain via Arrow[Fn1]
    Apply[Fn1[Boolean, ?]]
  }
  checkAll(apply.laws[Fn1[Boolean, ?]])

  // TODO - this is not legitimate. Laws seem to plug in Int wherever there's a hole?
  implicit val eqFn1IntBoolean = new Equal[Fn1[Int, Boolean]] {
    def equal(f1: Fn1[Int, Boolean], f2: Fn1[Int, Boolean]): Boolean =
      (0 to 100).toList.map(i => f1.run(i) == f2.run(i)).forall(identity _)
  }

  val DecidableFn1 = {
    import Arrow._
    implicit val boolMonoid = std.anyVal.booleanInstance.conjunction
    // obtain via Arrow[Fn1]
    Decidable[Fn1[?, Boolean]]
  }

  checkAll(decidable.laws[Fn1[?, Boolean]](DecidableFn1, implicitly, implicitly, implicitly))

  val DivideFn1 = {
    import Arrow._
    implicit val boolSemigroup: Semigroup[Boolean] = std.anyVal.booleanInstance.conjunction
    // obtain via Arrow[Fn1]
    Divide[Fn1[?, Boolean]]
  }

  checkAll(divide.laws[Fn1[?, Boolean]](DivideFn1, implicitly, implicitly, implicitly))
}
