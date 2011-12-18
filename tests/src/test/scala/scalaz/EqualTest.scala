package scalaz

import java.math.BigInteger
import scalacheck.{ScalazArbitrary}
import scalaz.scalacheck.ScalazProperties.{equal, order}
import std.AllInstances._
import Tags._
import ScalazArbitrary._
import java.util.concurrent.Callable

class EqualTest extends Spec {

  type A = String

  checkAll("NonEmptyList", equal.laws[NonEmptyList[A]])
  checkAll("() => A", equal.laws[() => A])
  checkAll("Tree", equal.laws[Tree[A]])
  checkAll("TreeLoc", equal.laws[TreeLoc[A]])
  checkAll("List", equal.laws[List[A]])
  checkAll("Stream", equal.laws[Stream[A]])
  checkAll("Callable", equal.laws[Callable[A]])
  checkAll("Zipper", equal.laws[Zipper[A]])
}
