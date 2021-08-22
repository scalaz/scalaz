package scalaz

import org.scalacheck.Prop.{exists, forAll, propBoolean}
import org.scalacheck.{Arbitrary, Properties, Shrink}

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Isomorphism.<~>
import Maybe.just
import std.anyVal._
import std.lazylist._
import std.tuple._
import std.option._
import syntax.contravariant._

object CorecursiveListTest extends SpecLite {
  type CL[A] = CorecursiveList[A]
  val CL = CorecursiveList
  checkAll(monadPlus.laws[CL])
  checkAll(foldable.laws[CL])
  checkAll(isEmpty.laws[CL])
  checkAll(align.laws[CL])
  checkAll(zip.laws[CL])
  checkAll(monoid.laws[CL[Int]])
  checkAll(order.laws[CL[Int]])
  checkAll(alt.laws[CL])

  "inequality exists" ! forAll {(a: CL[Int]) =>
    exists {(b: CL[Int]) =>
      propBoolean(!Equal[CL[Int]].equal(a, b))
    }
  }

  import EphemeralStreamTest.ephemeralStreamShow

  implicit def corecursiveListShow[A: Show]: Show[CL[A]] =
    Show[LazyList[A]] contramap (CL.lazyListIso.from(_))

  def isoTest[F[_], G[_]](iso: F <~> G)(
    implicit AF: Arbitrary[F[Int]], AG: Arbitrary[G[Int]],
    ShF: Shrink[F[Int]], ShG: Shrink[G[Int]],
    SF: Show[F[Int]], SG: Show[G[Int]],
    F: Equal[F[Int]], G: Equal[G[Int]]) = {
    val p = new Properties("iso roundtrip")
    p.property("f to g") = forAll{ (a: F[Int]) =>
      iso.from(iso.to(a)) must_===(a)
    }
    p.property("g to f") = forAll{ (a: G[Int]) =>
      iso.to(iso.from(a)) must_===(a)
    }
    p
  }

  checkAll("LazyList to corec iso", isoTest(CL.lazyListIso))

  checkAll("eph stream to corec iso", isoTest(CL.ephemeralStreamIso))

  "fromList" ! forAll { (xs: List[Int]) =>
    CL.fromList(xs) must_===(CL.fromLazyList(xs.to(LazyList)))
  }

  "fromVector" ! forAll { (xs: Vector[Int]) =>
    CL.fromVector(xs) must_===(CL.fromLazyList(xs.to(LazyList)))
  }

  def justDie[A](msg: String = "too strict!"): CL[A] =
    CL(()){_ => throw new TooStrictException(msg)}

  /** Emit one element, then blow up on the next one. */
  def oneAndDie[A](a: A, msg: String = "too strict!"): CL[A] =
    CL(true){s =>
      if (s) just((false, a)) else throw new TooStrictException(msg)
    }

  "ap is nonstrict" in {
    val sa = justDie[Unit]()
    val sb = justDie[Unit]()
    val sab = Apply[CL].tuple2(sa, sb)
    sab.step(sab.init).mustThrowA[TooStrictException]
  }

  "ap doesn't get ahead of itself" ! forAll {(a: Int, b: Int) =>
    val sa = oneAndDie(a, "sa too strict")
    val sb = oneAndDie(b, "sb too strict")
    val sab = Apply[CL].tuple2(sa, sb)
    sab.step(sab.init).map(_._2) must_===(just((a, b)))
  }

  "bind is nonstrict" in {
    val sa = justDie[Unit]()
    val sb = justDie[Unit]()
    val sab = Bind[CL].bind(sa){a => sb}
    sab.step(sab.init).mustThrowA[TooStrictException]
  }

  "bind doesn't get ahead of itself" ! forAll {(a: Int, b: Int) =>
    val sa = oneAndDie(a)
    val sb = oneAndDie(b)
    val sab = Bind[CL].bind(sa){a => Functor[CL].map(sb)((a, _))}
    sab.step(sab.init).map(_._2) must_===(just((a, b)))
  }

  "filter identity" ! forAll { (a: CL[Int]) =>
    MonadPlus[CL].filter(a)(_ => true) must_===(a)
  }

  "filter empty" ! forAll { (a: CL[Int]) =>
    MonadPlus[CL].filter(a)(_ => false) must_===(PlusEmpty[CL].empty)
  }

  "cons naturality" ! forAll {(x: Int, xs: CL[Int]) =>
    CL.lazyListIso.from(CL.cons(x, xs)) must_===(x #:: CL.lazyListIso.from(xs))
  }

  "index" ! forAll { (i: Int, xs: CL[Int]) =>
    val n = i.abs % 3
    Foldable[CL].index(xs, n) must_=== Foldable[CL].toList(xs).lift.apply(n)
  }

  "index infinite" ! forAll { (n: Int) =>
    val i = n.abs % 1000
    val xs = CL(0)(x => Maybe.just((x + 1, x)))
    Foldable[CL].index(xs, i) must_=== CL.lazyListIso.from(xs).lift.apply(i)
  }

  object instances {
    def equalAmbiguous[A: Order] = Equal[CL[A]]
  }
}

private final class TooStrictException(msg: String)
    extends RuntimeException(msg)
