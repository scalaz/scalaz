package scalaz
package example

import data._, typeclass._

object ShowUsage extends App {
  import Show._

  assert("asdf".show == "asdf")

  case class Foo(a: Int)
  implicit val fooShow: Show[Foo] = (f: Foo) => s"Foo! ${f.a}"

  assert(Foo(1).toString == "Foo(1)")
  assert(Foo(1).show == "Foo! 1")

  assert(Maybe.just(Foo(1)).show == "Just(Foo! 1)")

}
