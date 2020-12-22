package scalaz

import std.list._, std.tuple._, std.anyVal._
import Isomorphism._

object IsomorphismTest extends SpecLite {

  sealed trait Gaz
  case class Foo(s: String, i: Int) extends Gaz
  case class Bar(txt: String, num: Long) extends Gaz
  case class Baz() extends Gaz

  sealed trait Poly[A]
  case class PolyFoo[A](s: String, i: A) extends Poly[A]
  case class PolyBar() extends Poly[Unit]

  // TODO this breaks the OneOf derivation
  //case object Car extends Gaz

  implicit class toOps[A](val a: A) extends AnyVal {
    def to[B](implicit O: Iso[Function1, A, B]): B = O.to(a)
  }

  "case class derivation" ! {
    val (s, i) = Foo("hello", 1).to
    assert(Tag.unwrap(s) == "hello")
    assert(Tag.unwrap(i) == 1)
  }

  "sealed trait derivation" ! {
    (Bar("hello", 1) : Gaz).to match {
      case OneOf3.V2(Bar(s, i)) =>
        assert(s == "hello")
        assert(i == 1)
      case _ => assert(false)
    }
  }

  "polymorphic case clas derivation" ! {
    val (s, i) = PolyFoo("hello", 1).to
    assert(Tag.unwrap(s) == "hello")
    assert(Tag.unwrap(i) == 1)
  }

  // TODO polymorphic sealed traits don't seem to work
  // def testPolySealedTrait = {
  //   (PolyFoo("hello", 1) : Poly[Int]).to match {
  //     case OneOf2.V1(PolyFoo(s, i)) =>
  //       assert(s == "hello")
  //       assert(i == 1)
  //     case _ => assert(false)
  //   }
  // }

}


