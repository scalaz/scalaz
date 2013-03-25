package scalaz

import Scalaz._
import NonEmptyList._
import Zipper._
import syntax.equal._
import org.scalacheck._
import org.scalacheck.Prop._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class ZipperTest extends Spec {

  "Zipper From Stream" ! prop { (xs: Stream[Int]) =>
    (xs.toZipper map (_.toStream)).getOrElse(Stream()) === xs
  }

  "Zipper Move Then To Stream" in {
    val n = nels(1, 2, 3, 4)
    n.toZipper.move(2).map(_.toStream).exists(_ must be_===(n.stream))
  }

  "Next Affects Lengths" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.next)
        yield zn.lefts.length == z.lefts.length + 1 && zn.rights.length == z.rights.length - 1
    ) getOrElse (xs.length < 2)
  }

  "Previous Affects Lengths" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.previous)
        yield zn.lefts.length == z.lefts.length - 1 && zn.rights.length == z.rights.length + 1
    ) getOrElse (xs.length < 2)
  }

  "DeleteRight Affects Lengths" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.deleteRight)
        yield zn.rights.length == z.rights.length - 1
    ) getOrElse (xs.length < 2)
  }

  "DeleteRightC Affects Lengths" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.deleteRightC)
        yield zn.rights.length == z.rights.length - 1
    ) getOrElse (xs.length < 2)
  }

  "DeleteRight Affects Lengths and Moves Left if at end" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.deleteRight)
        yield zn.lefts.length == z.lefts.length - 1
    ) getOrElse (xs.length < 2)
  }

  "DeleteLeft Affects Lengths" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.deleteLeft)
        yield zn.lefts.length == z.lefts.length - 1
    ) getOrElse (xs.length < 2)
  }

  "DeleteLeftC Affects Lengths" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.deleteLeftC)
        yield zn.lefts.length == z.lefts.length - 1
    ) getOrElse (xs.length < 2)
  }

  "DeleteLeft Affects Lengths and Moves Right if at start" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.deleteLeft)
        yield zn.rights.length == z.rights.length - 1
    ) getOrElse (xs.length < 2)
  }

  "DeleteRightC Affects Lengths and Cycles to Start if at end" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.deleteRightC)
        yield zn.rights.length == z.lefts.length - 1
    ) getOrElse (xs.length < 2)
  }

  "DeleteLeftC Affects Lengths and Cycles to end if at start" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.deleteLeftC)
        yield zn.lefts.length == z.rights.length - 1
    ) getOrElse (xs.length < 2)
  }

  "Move" ! prop { (xs: Stream[Int], ys: Stream[Int], f: Int, n: Short) =>
    (
      zipper(xs, f, ys).move(n) map { (z: Zipper[Int]) =>
        z.lefts.length == xs.length + n &&
        z.rights.length == ys.length - n &&
        (n > 0 && ys(n - 1) == z.focus || n < 0 && xs(-(n + 1)) == z.focus || f == z.focus)
      }
    ) getOrElse (xs.length < (-n) || ys.length < n)
  }

  "Find" ! prop { (xs: Stream[Int], ys: Stream[Int], f: Int, n: Int, m: Int) =>
    val p = (i: Int) => i < n && i > m
    zipper(xs, f, ys).findZ(p) map { z => p(z.focus) } getOrElse !(xs.find(p).isDefined || ys.find(p).isDefined || p(f))
  }

  "Update Modifies Zipper Correctly" ! prop { (xs: Stream[Int], ys: Stream[Int], f: Int, u: Int) =>
    Equal[Zipper[Int]].equal(zipper(xs, f, ys).update(u), zipper(xs, u, ys))
  }

  "Start" ! prop { (xs: Stream[Int], ys: Stream[Int], f: Int) =>
    val zo = zipper(xs, f, ys)
    val z = zo.start
    z.lefts.length === 0 &&
      z.rights.length === z.length - 1 &&
      zo.move(-xs.length).exists(Equal[Zipper[Int]].equal(_, z)) &&
      (z.move(xs.length).map(Equal[Zipper[Int]].equal(_, zo)).getOrElse(z.length == 0): Boolean)
  }

  "End" ! prop { (xs: Stream[Int], ys: Stream[Int], f: Int) =>
    val zo = zipper(xs, f, ys)
    val z = zo.end
    z.lefts.length === z.length - 1 &&
      z.rights.length === 0 &&
      zo.move(ys.length).exists(Equal[Zipper[Int]].equal(_, z)) &&
      (z.move(-ys.length).map(Equal[Zipper[Int]].equal(_, zo)).getOrElse(z.length == 0): Boolean)
  }

  checkAll("Zipper", equal.laws[Zipper[Int]])

  {
    implicit def zipperEqual[A: Equal]: Equal[Zipper[A]] = new Equal[Zipper[A]] {
      import std.stream.streamEqual
      def streamEqualApprox = streamEqual[A].contramap((_: Stream[A]).take(1000))
      def equal(a1: Zipper[A], a2: Zipper[A]) =
        streamEqualApprox.equal(a1.lefts, a2.lefts) &&
          Equal[A].equal(a1.focus, a2.focus) &&
          streamEqualApprox.equal(a1.rights, a2.rights)
    }

    "Zipper.traverse" in {
      skipped("stack overflow in sequential fusion")
      //checkAll("Zipper", traverse.laws[Zipper])
      // dummy
      false
    }

    checkAll("Zipper", applicative.laws[Zipper])
    checkAll("Zipper", comonad.laws[Zipper])
  }
}

// vim: expandtab:ts=2:sw=2
