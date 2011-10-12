package scalaz.example

class TraverseUsage {
  import scalaz._

  // type class instances
  import std.List._
  import std.Option._
  import Ident.id

  // syntax
  import syntax.Syntax.applicative._

  // traverse usages

  // through the strict identity monad (type Id[x] = x)
  // The type class Monad[ID] overrides methods like traverse for efficiency (see Idents.scala)
  // The type annotation is needed, unfortunately.
  val ids: List[Int] = List(1, 2, 3).traverse[Id, Int](_ * 2)

  // Through the Option effect
  val opts1: Option[List[Int]] = List(1, 2, 3).traverse[Option, Int](x => some(x * 2))
  val opts2: Option[List[Int]] = List(1, 2, 3).traverse(x => some(x * 2))

  // IO not yet in this branch, but would follow the same pattern.
}