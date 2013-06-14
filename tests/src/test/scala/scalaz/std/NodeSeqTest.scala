package scalaz
package std

import std.nodeseq._
import syntax.apply._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import org.scalacheck.{ Gen, Arbitrary }

import scala.xml.{ Node, NodeSeq, XML }

class NodeSeqTest extends Spec {
  {
    def nonEmptyAlphaStr: Gen[String] = for(cs <- Gen.listOf1(Gen.alphaChar)) yield cs.mkString

    val genNode = ^ (
      nonEmptyAlphaStr, nonEmptyAlphaStr
    ) { (s1, s2) ⇒ XML.loadString("<" + s1 + ">" + s2 + "</" + s1 + ">") }

    val genNodeSeq = Gen.containerOf[Array, Node](genNode)
    implicit val NodeSeqArb = Arbitrary[NodeSeq](genNodeSeq map (_.toSeq))

    checkAll(monoid.laws[NodeSeq])
    checkAll(equal.laws[NodeSeq])
  }
}
