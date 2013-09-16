package scalaz

import Scalaz._
import NonEmptyList._
import Zipper._
import syntax.equal._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.specs2.matcher.ExceptionMatchers
import org.scalacheck.{Prop, Gen, Arbitrary}
import org.scalacheck.Arbitrary._

class ZipperTest extends Spec with ExceptionMatchers {

  "Zipper From Stream" ! prop { (xs: Stream[Int]) =>
    (xs.toZipper map (_.toStream)).getOrElse(Stream()) === xs
  }

  "Mapping on a zipper should be the same as mapping on the elements of the stream" ! prop { (xs: Stream[Int], a: Int) =>
    val fun: Int => Int = _ + a

    (
      for (z <- xs.toZipper; zM <- (xs map fun).toZipper) yield z.map(fun) == zM
    ) getOrElse (xs.length < 1)
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

  "nextC moves focus or loops" ! prop { z: Zipper[Int] =>
    val zn = z.nextC
    zn.toStream == z.toStream && (
      if (z.atEnd) zn.atStart 
      else zn.index == z.index + 1
    )
  }

  "Next changes focus, lefts and rights " ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    if (r.length > 0) zipper(l, f, r).next.cata(
      (nextZipper => 
        nextZipper.focus == r(0) &&
        nextZipper.lefts == f +: l &&
        nextZipper.rights == r.tail),
      false
    ) else {
      !zipper(l, f, r).next.isDefined
    }
  }

  "Zipper next returns Some when rights is nonempty, none otherwise." ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    if (r.length > 0) zipper(l, f, r).next.isDefined 
    else !zipper(l, f, r).next.isDefined 
  }

  "Zipper nextOr returns a new zipper when used on empty rights or Some of next" ! prop { 
    (l: Stream[Int], f: Int, r: Stream[Int], alt: Zipper[Int]) =>

    val z = zipper(l, f, r)
    if (r.length > 0) {
      z.next.cata(zNext => zNext == z.nextOr(alt), false)
    } else {
      z.nextOr(alt) == alt
    }
  }

  "Zipper tryNext returns Some of next or throws" ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    val z = zipper(l, f, r)
    if (r.length > 0) {
      z.next.cata(zNext => zNext == z.tryNext, false)
    } else {
      try { 
        z.tryNext 
        false 
      } catch {
        case r: RuntimeException => true
      } 
    }
  }

  "Previous Affects Lengths" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.previous)
        yield zn.lefts.length == z.lefts.length - 1 && zn.rights.length == z.rights.length + 1
    ) getOrElse (xs.length < 2)
  }

  "previousC moves focus or loops" ! prop { z: Zipper[Int] =>
    val zp = z.previousC
    zp.toStream == z.toStream && (
      if (z.atStart) zp.atEnd 
      else zp.index == z.index - 1
    )
  }

  "Previous changes the focus, lefts and rights " ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    if (l.length > 0) zipper(l, f, r).previous.cata(
      (prevZipper => 
        prevZipper.focus == l(0) &&
        prevZipper.lefts == l.tail &&
        prevZipper.rights == f +: r),
      false)
    else {
      !zipper(l, f, r).previous.isDefined
    }
  }

  "Zipper previousOr returns a new zipper when used on empty rights or Some of next" ! prop { 
    (l: Stream[Int], f: Int, r: Stream[Int], alt: Zipper[Int]) =>

    val z = zipper(l, f, r)
    if (l.length > 0) {
      z.previous.cata(zNext => zNext == z.previousOr(alt), false)
    } else {
      z.previousOr(alt) == alt
    }
  }

  "Zipper tryPrevious returns Some of next or throws" ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    val z = zipper(l, f, r)
    if (l.length > 0) {
      z.previous.cata(zPrev => zPrev == z.tryPrevious, false)
    } else {
      try { 
        z.tryPrevious 
        false 
      } catch { 
        case r: RuntimeException => true
      } 
    }
  }

  def insertionTest(
      name: String,
      insertion: (Zipper[Int], Int) => Zipper[Int],
      pred: (Zipper[Int], Zipper[Int], Int) => Boolean) = name ! prop { (z: Zipper[Int], e: Int) =>

    val zi = insertion(z, e)
    pred(zi, z, e)
  }

  val leftAndFocusChanged: (Zipper[Int], Zipper[Int], Int) => Boolean = { (zNew, zOld, newFocus) =>
    zNew.focus == newFocus &&
    zNew.lefts.head == zOld.focus &&
    zNew.lefts.tail == zOld.lefts &&
    zNew.rights == zOld.rights
  }

  val rightAndFocusChanged: (Zipper[Int], Zipper[Int], Int) => Boolean = { (zNew, zOld, newFocus) =>
    zNew.focus == newFocus &&
    zNew.lefts == zOld.lefts &&
    zNew.rights.head == zOld.focus &&
    zNew.rights.tail == zOld.rights 
  }

  insertionTest("insertRight changes focus and appends to lefts", (z, e) => z.insertRight(e), leftAndFocusChanged)
  insertionTest("insert changes focus and appends to lefts",      (z, e) => z.insert(e),      leftAndFocusChanged)
  insertionTest("insertLeft changes focus and appends to lefts",  (z, e) => z.insertLeft(e),  rightAndFocusChanged)

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

  "deleteRightC moves the focus to the right or if not possible to the first element" ! prop { z: Zipper[Int] =>
    (
      for {
        zd <- z.deleteRightC
      } yield {
        if (z.rights.length > 0) zd.focus == z.rights(0)
        else                     zd.focus == z.lefts.last
      }
    ) getOrElse(z.lefts.isEmpty && z.rights.isEmpty)
  }

  "deleteRightCOr should return Some of deleteLeftC or an alternative" ! prop { (z: Zipper[Int], alt: Zipper[Int]) =>
    val zd = z.deleteRightCOr(alt)
    if (z.lefts.length == 0 && z.rights.length == 0) zd == alt
    else z.deleteRightC.cata(z => z == zd, false)
  }

  "DeleteRight Affects Lengths and Moves Left if at end" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.deleteRight)
        yield zn.lefts.length == z.lefts.length - 1
    ) getOrElse (xs.length < 2)
  }

  "deleteRight moves the focus to the right or if not possible left" ! prop { z: Zipper[Int] =>
    (
      for {
        zd <- z.deleteRight
      } yield {
        if (z.rights.length > 0) zd.focus == z.rights(0)
        else                     zd.focus == z.lefts(0)
      }
    ) getOrElse(z.lefts.isEmpty && z.rights.isEmpty)
  }

  "deleteRightOr should return Some of deleteLeft or an alternative" ! prop { (z: Zipper[Int], alt: Zipper[Int]) =>
    val zd = z.deleteRightOr(alt)
    if (z.lefts.length == 0 && z.rights.length == 0) zd == alt
    else {
      if (z.rights.length != 0)  zd.rights == z.rights.tail && zd.lefts == z.lefts
      else                       zd.rights.isEmpty && zd.lefts == z.lefts.tail
    }
  }

  "DeleteLeft Affects Lengths" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.deleteLeft)
        yield zn.lefts.length == z.lefts.length - 1
    ) getOrElse (xs.length < 2)
  }

  "deleteLeft moves the focus to the left or if not possible right" ! prop { z: Zipper[Int] =>
    (
      for {
        zd <- z.deleteLeft
      } yield {
        if (z.lefts.length > 0) zd.focus == z.lefts(0)
        else                    zd.focus == z.rights(0)
      }
    ) getOrElse(z.lefts.isEmpty && z.rights.isEmpty)
  }

  "DeleteLeftC Affects Lengths" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.deleteLeftC)
        yield zn.lefts.length == z.lefts.length - 1
    ) getOrElse (xs.length < 2)
  }
 
  "deleteLeftC moves the focus to the left or if not possible to the last element" ! prop { z: Zipper[Int] =>
    (
      for {
        zd <- z.deleteLeftC
      } yield {
        if (z.lefts.length > 0) zd.focus == z.lefts(0)
        else                    zd.focus == z.rights.last
      }
    ) getOrElse(z.lefts.isEmpty && z.rights.isEmpty)
  }

  "deleteLeftCOr should return Some of deleteLeftC or an alternative" ! prop { (z: Zipper[Int], alt: Zipper[Int]) =>
    val zd = z.deleteLeftCOr(alt)
    if (z.lefts.length == 0 && z.rights.length == 0) zd == alt
    else z.deleteLeftC.cata(z => z == zd, false)
  }

  "deleteRightOr should return Some of deleteLeft or an alternative" ! prop { (z: Zipper[Int], alt: Zipper[Int]) =>
    val zd = z.deleteRightOr(alt)
    if (z.lefts.length == 0 && z.rights.length == 0) zd == alt
    else {
      if (z.rights.length != 0)  zd.rights == z.rights.tail && zd.lefts == z.lefts
      else                       zd.rights.isEmpty && zd.lefts == z.lefts.tail
    }
  }

  "DeleteLeft Affects Lengths and Moves Right if at start" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.deleteLeft)
        yield zn.rights.length == z.rights.length - 1
    ) getOrElse (xs.length < 2)
  }

  "deleteLeftOr should return Some of deleteLeft or an alternative" ! prop { (z: Zipper[Int], alt: Zipper[Int]) =>
    if (z.lefts.length == 0 && z.rights.length == 0) z.deleteLeftOr(alt) == alt
    else {
      val zd = z.deleteLeftOr(alt)
      if (z.lefts.length != 0)  zd.lefts == z.lefts.tail && zd.rights == z.rights
      else                      zd.lefts.isEmpty && zd.rights == z.rights.tail
    }
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

  "move should not cause a stackoverflow error" in {
    val size = 32 * 1024 
    (
      for {
        z <- Stream.from(1).take(size).toZipper
        n = size - 1
        zm <- z.move(n)
      } yield { zm.focus == n + 1 }
    ) getOrElse(false)
  }

  "moveOr should return some of move or an alternative" ! prop { 
    (l: Stream[Int], f: Int, r: Stream[Int], n: Short, alt: Zipper[Int]) =>

    val z = zipper(l, f, r).moveOr(n, alt)
    if (l.length < (-n) || r.length < n) z == alt
    else {
      z.lefts.length == l.length + n &&
      z.rights.length == r.length - n &&
      (n > 0 && r(n - 1) == z.focus || n < 0 && l(-(n + 1)) == z.focus || f == z.focus)
    }
  }

  "Length should return the size of the zipper" ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    zipper(l, f, r).length == l.length + 1 + r.length
  }

  "The zipper should be atStart when the lefts stream is empty" ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    if (zipper(l, f, r).atStart) l.isEmpty
    else !l.isEmpty
  }

  "withFocus should pair only the focus with true, false otherwise" ! prop { z: Zipper[Int] =>
    val zf = z.withFocus
    zf.lefts.find(_._2).isEmpty && zf.focus._2 && zf.rights.find(_._2).isEmpty
  }

  "start should set the zipper at the start" ! prop { z: Zipper[Int] =>
    val zs = z.start
    zs.toStream == z.toStream && zs.index == 0
  }

  "end should set the zipper at the end" ! prop { z: Zipper[Int] =>
    val ze = z.end
    ze.toStream == z.toStream && ze.index == ze.length - 1
  }

  "The zipper should be atEnd when the right stream is empty" ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    if (zipper(l, f, r).atEnd) r.isEmpty
    else !r.isEmpty
  }

  "Find" ! prop { (xs: Stream[Int], ys: Stream[Int], f: Int, n: Int, m: Int) =>
    val p = (i: Int) => i < n && i > m
    zipper(xs, f, ys).findZ(p) map { z => p(z.focus) } getOrElse !(xs.find(p).isDefined || ys.find(p).isDefined || p(f))
  }

  "findZ shouldn't change elements" ! prop { (xs: Stream[Int], ys: Stream[Int], f: Int, n: Int, m: Int) =>
    val p = (i: Int) => i < n && i > m
    zipper(xs, f, ys).findZ(p).map { 
      z => z.toStream == zipper(xs, f, ys).toStream 
    } getOrElse !(xs.find(p).isDefined || ys.find(p).isDefined || p(f))
  }

  "findZor returns some of find or an alternative" ! prop { 
    (z: Zipper[Int], n: Int, m: Int, alt: Zipper[Int]) =>
    val p = (i: Int) => i < n && i > m

    if (z.lefts.find(p).isDefined || p(z.focus) || z.rights.find(p).isDefined) {
      p(z.findZor(p, alt).focus)
    } else {
      z.findZor(p, alt) == alt
    }
  }

  "findZ should not cause a stackoverflow error" in {
    val size = 32 * 1024
    (
      for {
        z <- Stream.from(1).take(size).toZipper
        elem = size - 1
        zf <- z.findZ(_ == elem)
      } yield zf.focus == elem
    ) getOrElse(false)
  }

  "findBy if given a function that returns None should not return anything" ! prop { z: Zipper[Int] =>
    z.findBy(z => None)(x => x == z.focus).isEmpty
  }

  val intZipperWithExistingElement: Gen[(Zipper[Int], Int)] = for {
    z <- arbitrary[Zipper[Int]]
    stream = z.toStream
    i <- Gen.choose(0, stream.length -1)
  } yield (z, stream(i))

  "given nextC findBy should return Some if the element exists" !  Prop.forAll(intZipperWithExistingElement) { case (z, e) =>
    z.findBy(z => some(z.nextC))(x => x == e).isDefined
   }

  "findBy should not blow the stack" !  prop { z: Zipper[Int] =>
    var limit = 10 * 1000
    z.findBy(z => if (limit > 0) { limit -= 1; some(z.nextC) } else none)(x => false)
    true
   }

  def minSizeIntZipper(size: Int): Gen[Zipper[Int]] = for {
      leftSize <- Gen.choose(0, size - 2)
      rightSize = size - 1 - leftSize
      lefts  <- Gen.containerOfN[Stream,Int](leftSize,  implicitly[Arbitrary[Int]].arbitrary)
      rights <- Gen.containerOfN[Stream,Int](rightSize, implicitly[Arbitrary[Int]].arbitrary)
      focus <- arbitrary[Int]
  } yield zipper(lefts, focus, rights)

  "findNext should not blow the stack" !  Prop.forAll(minSizeIntZipper(10 * 1000)) { z =>
    var limit = 10 * 1000
    z.start.findNext { x => limit -= 1; limit > 0 }
    true
   }

  "findPrevious should not blow the stack" !  Prop.forAll(minSizeIntZipper(10 * 1000)) { z =>
    var limit = 10 * 1000
    z.end.findPrevious { x => limit -= 1; limit > 0 }
    true
   }

  "Update Modifies Zipper Correctly" ! prop { (xs: Stream[Int], ys: Stream[Int], f: Int, u: Int) =>
    Equal[Zipper[Int]].equal(zipper(xs, f, ys).update(u), zipper(xs, u, ys))
  }

  "Modify Modifies Zipper Correctly" ! prop { (xs: Stream[Int], ys: Stream[Int], f: Int, u: Int) =>
    val modF: Int => Int = _ + u
    Equal[Zipper[Int]].equal(zipper(xs, f, ys).modify(modF), zipper(xs, f + u, ys))
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

  "positions should return a zippers with focus on this" ! prop { z: Zipper[Int] =>
    z.positions.focus == z
  }

  "positions should return a zippers with all possible positions of a zipper" ! prop { z: Zipper[Int] =>
    val indeces = z.positions.map { z => z.index }.toStream
    indeces.min == 0 && indeces.max == z.length -1 && indeces.sorted == indeces &&
    z.positions.map { z => z.toStream }.toStream.distinct.length == 1
  }

  "index returns the position of the focus" ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    zipper(l, f, r).index == l.length
  }

  checkAll("Zipper", equal.laws[Zipper[Int]])
  checkAll("Zipper", traverse.laws[Zipper])
  checkAll("Zipper", comonad.laws[Zipper])

  {
    implicit def zipperEqual[A: Equal]: Equal[Zipper[A]] = new Equal[Zipper[A]] {
      import std.stream.streamEqual
      def streamEqualApprox = streamEqual[A].contramap((_: Stream[A]).take(1000))
      def equal(a1: Zipper[A], a2: Zipper[A]) =
        streamEqualApprox.equal(a1.lefts, a2.lefts) &&
          Equal[A].equal(a1.focus, a2.focus) &&
          streamEqualApprox.equal(a1.rights, a2.rights)
    }

    checkAll("Zipper", applicative.laws[Zipper])
  }
}

// vim: expandtab:ts=2:sw=2
