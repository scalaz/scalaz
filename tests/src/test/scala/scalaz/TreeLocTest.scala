package scalaz

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties._
import scalaz.std.AllInstances._

object TreeLocTest extends SpecLite {

  checkAll("TreeLoc", order.laws[TreeLoc[Int]])
  checkAll("TreeLoc", traverse1.laws[TreeLoc])
  checkAll(FoldableTests.anyAndAllLazy[TreeLoc])

  "ScalazArbitrary.treeLocGenSized" ! forAll(Gen.choose(1, 200)) { size =>
    val gen = treeLocGenSized[Unit](size)
    Stream.continually(gen.sample).flatten.take(10).map(Foldable[TreeLoc].length(_)).forall(_ == size)
  }

  {
    def treeEqual[A: Equal]: Equal[Tree[A]] = new Equal[Tree[A]] {

      import std.stream.streamEqual

      def streamEqualApprox = streamEqual[Tree[A]].contramap((_: Stream[Tree[A]]).take(1000))

      def equal(a1: Tree[A], a2: Tree[A]) =
        Equal[A].equal(a1.rootLabel, a2.rootLabel) && streamEqualApprox.equal(a1.subForest, a2.subForest)
    }

    // TODO checkAll("TreeLoc", applicative.laws[TreeLoc])
    checkAll("TreeLoc", comonad.laws[TreeLoc])
  }

  "TreeLoc from empty forest does not throw an exception" ! {
    import scalaz.std.option._
    val result: Option[TreeLoc[Int]] = TreeLoc.fromForest(Stream.empty[Tree[Int]])
    result must_==(none[TreeLoc[Int]])
  }


  object instances {
    def equal[A: Equal] = Equal[TreeLoc[A]]

    def order[A: Order] = Order[TreeLoc[A]]

    // checking absence of ambiguity
    def equal[A: Order] = Equal[TreeLoc[A]]
  }

}
