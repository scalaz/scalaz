package scalaz

import org.scalacheck.Prop.{exists, forAll, propBoolean}
import org.scalacheck.{Arbitrary, Prop, Properties, Shrink}

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Isomorphism.<~>
import std.anyVal._
import std.stream._
import syntax.contravariant._

object CorecursiveListTest extends SpecLite {
  type CL[A] = CorecursiveList[A]
  checkAll(monadPlus.laws[CL])
  checkAll(foldable.laws[CL])
  checkAll(zip.laws[CL])
  checkAll(monoid.laws[CL[Int]])
  checkAll(order.laws[CL[Int]])

  "inequality exists" ! forAll {(a: CL[Int]) =>
    exists {(b: CL[Int]) =>
      propBoolean(!Equal[CL[Int]].equal(a, b))
    }
  }

  import EphemeralStreamTest.ephemeralStreamShow

  implicit def corecursiveListShow[A: Show]: Show[CL[A]] =
    Show[Stream[A]] contramap (CorecursiveList.streamIso.from(_))

  def isoTest[F[_], G[_]](iso: F <~> G)(
    implicit AF: Arbitrary[F[Int]], AG: Arbitrary[G[Int]],
    ShF: Shrink[F[Int]], ShG: Shrink[G[Int]],
    SF: Show[F[Int]], SG: Show[G[Int]],
    F: Equal[F[Int]], G: Equal[G[Int]]) = {
    val p = new Properties("iso roundtrip")
    p.property("f to g") = forAll{a: F[Int] =>
      iso.from(iso.to(a)) must_===(a)
    }
    p.property("g to f") = forAll{a: G[Int] =>
      iso.to(iso.from(a)) must_===(a)
    }
    p
  }

  "stream to corec iso" ! isoTest(CorecursiveList.streamIso)

  "eph stream to corec iso" ! isoTest(CorecursiveList.ephemeralStreamIso)

  object instances {
    def equalAmbiguous[A: Order] = Equal[CL[A]]
  }
}
