package scalaz.example

import scalaz._

object MixedBag extends App {
  monoid()
  traverseBigList()
  traverseBigStream()

  def monoid() {
    import std.anyVal._
    import std.option._

    import syntax.monoid._
    
    1 |+| 2
    1 mappend 2
    some(1) |+| some(2)
    some(1) |+| mzero[Option[Int]]

    intInstance.semigroupSyntax.mappend(1, 2)
  }

  def traverseBigList() {
    import std.option._
    import std.list._
    import syntax.traverse._

    val xs: Option[List[Int]] = (1 to 100000 toList).traverse(x => some(x * 2))
    println(xs map (_ take 10))
  }

  def traverseBigStream() {
//    import std.Option._
//    import std.Stream._
//    import syntax.applicative._
//
//    (1 to 100000 toStream).traverse(x => some(x * 2))
  }
}