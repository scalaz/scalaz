package scalaz.example

import scalaz._

object MixedBag extends App {
  monoid()
  traverseBigList()
  traverseBigStream()
  tree()

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

  def tree() {
    import std.string._
    import syntax.semigroup._
    import syntax.equal._
    import syntax.tree._
    import syntax.traverse._
    import std.stream._

    val tree: Tree[Int] = 1.node(2.node(3.leaf), 4.leaf, 5.leaf)
    val r = tree.foldRight(".")(i => s => i.toString |+| s)
    r assert_=== "12345."
    val f = tree.flatten.foldMap(_.toString)
    f assert_=== "12345"
    val m = tree.foldMap(_.toString)
    m assert_=== "12345"
  }
}