package scalaz

import std.AllInstances._
import scalaz.testlib.ScalazProperties._
import scalaz.testlib.ScalazArbitrary._

class TreeLocTest extends testlib.Spec {

  checkAll("TreeLoc", equal.laws[Tree[Int]])

  {
    def treeEqual[A: Equal]: Equal[Tree[A]] = new Equal[Tree[A]] {
      import std.stream.streamEqual
      def streamEqualApprox = streamEqual[Tree[A]].contramap((_: Stream[Tree[A]]).take(1000))
      def equal(a1: Tree[A], a2: Tree[A]) =
        Equal[A].equal(a1.rootLabel, a2.rootLabel) && streamEqualApprox.equal(a1.subForest, a2.subForest)
    }
    implicit def treeLocEqual[A: Equal]: Equal[TreeLoc[A]] = treeEqual[A].contramap((_: TreeLoc[A]).toTree)

    // TODO checkAll("TreeLoc", traverse.laws[TreeLoc])
    // TODO checkAll("TreeLoc", applicative.laws[TreeLoc])
    checkAll("TreeLoc", comonad.laws[TreeLoc])
  }
}
