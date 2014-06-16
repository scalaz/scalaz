package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object DisjunctionTest extends SpecLite {

  checkAll(order.laws[Int \/ Int])
  checkAll(monoid.laws[Int \/ Int])
  checkAll(monad.laws[({type λ[α] = Int \/ α})#λ])
  checkAll(plus.laws[({type λ[α] = Int \/ α})#λ])
  checkAll(traverse.laws[({type λ[α] = Int \/ α})#λ])
  checkAll(bitraverse.laws[\/])
  checkAll(monadError.laws[\/, Int])

  "fromTryCatchThrowable" in {
    class Foo extends Throwable
    final class Bar extends Foo
    val foo = new Foo
    val bar = new Bar

    implicit val equalFoo = Equal.equalA[Foo]
    implicit val showFoo = Show.showA[Foo]
    implicit val equalBar = Equal.equalA[Bar]
    implicit val showBar = Show.showA[Bar]

    \/.fromTryCatchThrowable[Int, Foo](1) must_=== \/.right(1)
    \/.fromTryCatchThrowable[Int, Foo](throw foo) must_=== \/.left(foo)
    \/.fromTryCatchThrowable[Int, Foo](throw bar) must_=== \/.left(bar)
    \/.fromTryCatchThrowable[Int, Bar](throw foo).mustThrowA[Foo]
  }
}
