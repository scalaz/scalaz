package scalaz

import BijectionT.{Bijection, liftBijection}
import Id.Id
import Isomorphism.{<=>, IsoSet}
import std.AllInstances._
import std.option.some
import syntax.invariantFunctor._
import scalaz.scalacheck.ScalazProperties._

class InvariantFunctorTest extends Spec {

  "xmap" in {
    some(1).xmap[Int](_ + 1, _ - 1) must be_===(some(2))
  }

  "xmap iso" in {
    val succI: Int <=> Int = new IsoSet[Int, Int] {
      def to = (_: Int) + 1
      def from = (_: Int) - 1
    }
    some(1) xmapi succI must be_===(some(2))
  }

  "xmap bijection" in {
    val succB: Bijection[Int, Int] = liftBijection[Id, Id, Int, Int](_ + 1, _ - 1)
    some(1) xmapb succB must be_===(some(2))
  }

  case class Num(x: Int)
  implicit val showNum = Show.showA[Num]
  implicit val eqNum = Equal.equalA[Num]

  "semigroup" in {
    val sg: Semigroup[Num] = Semigroup[Int].xmap[Num](Num.apply _, _.x)
    sg.append(Num(1), Num(2)) must be_===(Num(3))
  }

  "monoid" in {
    val sg: Monoid[Num] = Monoid[Int].xmap[Num](Num.apply _, _.x)
    sg.append(Num(1), Num(2)) must be_===(Num(3))
    sg.zero must be_===(Num(0))
  }
}
