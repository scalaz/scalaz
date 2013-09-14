package scalaz

import Scalaz._
import NonEmptyList.nels
import Zipper._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.specs2.matcher.{ExceptionMatchers, NumericMatchers, MatchResult}
import org.specs2.matcher.TraversableMatchers.contain
import org.specs2.matcher.AnyMatchers.{beTrue, beFalse}
import org.specs2.matcher.OptionMatchers.beSome
import org.scalacheck.{Prop, Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary

class ZipperTest extends Spec with ExceptionMatchers with NumericMatchers {

  "Zipper From Stream" ! prop { (xs: Stream[Int]) =>
    (xs.toZipper map (_.toStream)).getOrElse(Stream()) === xs
  }

  "Mapping on a zipper should be the same as mapping on the elements of the stream" ! prop { (xs: Stream[Int], a: Int) =>
    val fun: Int => Int = _ + a

    (
      for (z <- xs.toZipper; zM <- (xs map fun).toZipper) yield z.map(fun) must be_===(zM)
    ) getOrElse { xs.length must be_===(0) }
  }

  "Zipper Move Then To Stream" in {
    val n = nels(1, 2, 3, 4)
    n.toZipper.move(2).map(_.toStream).exists(_ must be_===(n.stream))
  }

  "Next Affects Lengths" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.next)
        yield {
        (zn.lefts.length must be_===(z.lefts.length + 1)) and (zn.rights.length must be_===(z.rights.length - 1))
      }
    ) getOrElse { xs.length must be_<(2) }
  }

  "nextC moves focus or loops" ! prop { z: Zipper[Int] =>
    val zn = z.nextC
    zn.toStream must be_===(z.toStream)

    if (z.atEnd) zn.atStart must beTrue
    else zn.index must be_===(z.index + 1)
  }

  "Next changes focus, lefts and rights " ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    if (r.length > 0) {
      zipper(l, f, r).next must beSome.like{ case nextZipper =>
        nextZipper.focus must be_===(r(0))
        nextZipper.lefts must be_===(f +: l)
        nextZipper.rights must be_===(r.tail)
      }
    } else {
      zipper(l, f, r).next.isEmpty must beTrue
    }
  }

  "Zipper next returns Some when rights is nonempty, none otherwise." ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    if (r.length > 0) zipper(l, f, r).next.isDefined must beTrue
    else zipper(l, f, r).next.isDefined must beFalse
  }

  "Zipper nextOr returns a new zipper when used on empty rights or Some of next" ! prop { 
    (l: Stream[Int], f: Int, r: Stream[Int], alt: Zipper[Int]) =>

    val z = zipper(l, f, r)
    if (r.length > 0) {
      z.next must be_===(Some(z.nextOr(alt)))
    } else {
      z.nextOr(alt) must be_===(alt)
    }
  }

  "Zipper tryNext returns Some of next or throws" ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    val z = zipper(l, f, r)
    if (r.length > 0) {
      z.next must be_===(Some(z.tryNext))
    } else {
      try { 
        z.tryNext 
        ko
      } catch {
        case r: RuntimeException => ok
      } 
    }
  }

  "Previous Affects Lengths" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.previous)
        yield zn.lefts.length must be_===(z.lefts.length - 1) and (zn.rights.length must be_===(z.rights.length + 1))
    ) getOrElse { xs.length must be_<(2) }
  }

  "previousC moves focus or loops" ! prop { z: Zipper[Int] =>
    val zp = z.previousC
    zp.toStream must be_===(z.toStream)

    if (z.atStart) zp.atEnd must beTrue
    else zp.index must be_===(z.index - 1)
  }

  "Previous changes the focus, lefts and rights " ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    if (l.length > 0) 
      zipper(l, f, r).previous must beSome.like{ case prevZipper => 
        prevZipper.focus must be_===(l(0))
        prevZipper.lefts must be_===(l.tail)
        prevZipper.rights must be_===(f +: r)
      }
    else {
      zipper(l, f, r).previous.isDefined must beFalse
    }
  }

  "Zipper previousOr returns a new zipper when used on empty rights or Some of next" ! prop { 
    (l: Stream[Int], f: Int, r: Stream[Int], alt: Zipper[Int]) =>

    val z = zipper(l, f, r)
    if (l.length > 0) {
      z.previous must be_===(Some(z.previousOr(alt)))
    } else {
      z.previousOr(alt) must be_===(alt)
    }
  }

  "Zipper tryPrevious returns Some of next or throws" ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    val z = zipper(l, f, r)
    if (l.length > 0) {
      z.previous must be_===(Some(z.tryPrevious))
    } else {
      try { 
        z.tryPrevious 
        ko 
      } catch { 
        case r: RuntimeException => ok
      } 
    }
  }

  def insertionTest(
      name: String,
      insertion: (Zipper[Int], Int) => Zipper[Int],
      pred: (Zipper[Int], Zipper[Int], Int) => MatchResult[_]) = name ! prop { (z: Zipper[Int], e: Int) =>

    val zi = insertion(z, e)
    pred(zi, z, e)
  }

  val leftAndFocusChanged: (Zipper[Int], Zipper[Int], Int) => MatchResult[_] = { (zNew, zOld, newFocus) =>
    {zNew.focus must be_===(newFocus)} and
    {zNew.lefts.head  must be_===(zOld.focus)} and
    {zNew.lefts.tail must be_===(zOld.lefts)} and
    {zNew.rights must be_===(zOld.rights)}
  }

  val rightAndFocusChanged: (Zipper[Int], Zipper[Int], Int) => MatchResult[_] = { (zNew, zOld, newFocus) =>
    {zNew.focus must be_===(newFocus)} and
    {zNew.lefts must be_===(zOld.lefts)} and
    {zNew.rights.head must be_===(zOld.focus)} and
    {zNew.rights.tail must be_===(zOld.rights)}
  }

  insertionTest("insertRight changes focus and appends to lefts", (z, e) => z.insertRight(e), leftAndFocusChanged)
  insertionTest("insert changes focus and appends to lefts",      (z, e) => z.insert(e),      leftAndFocusChanged)
  insertionTest("insertLeft changes focus and appends to lefts",  (z, e) => z.insertLeft(e),  rightAndFocusChanged)

  "DeleteRight Affects Lengths" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.deleteRight)
        yield zn.rights.length must be_===(z.rights.length - 1)
    ) getOrElse {xs.length must be_<(2) }
  }

  "DeleteRightC Affects Lengths" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.deleteRightC)
        yield zn.rights.length must be_===(z.rights.length - 1)
    ) getOrElse {xs.length must be_<(2) }
  }

  "deleteRightC moves the focus to the right or if not possible to the first element" ! prop { z: Zipper[Int] =>
    (
      for {
        zd <- z.deleteRightC
      } yield {
        if (z.rights.length > 0) zd.focus must be_===(z.rights(0))
        else                     zd.focus must be_===(z.lefts.last)
      }
    ) getOrElse{ (z.lefts.isEmpty must beTrue) and (z.rights.isEmpty must beTrue) }
  }

  "deleteRightCOr should return Some of deleteLeftC or an alternative" ! prop { (z: Zipper[Int], alt: Zipper[Int]) =>
    val zd = z.deleteRightCOr(alt)
    if (z.lefts.length == 0 && z.rights.length == 0) zd must be_===(alt)
    else z.deleteRightC must be_===(Some(zd))
  }

  "DeleteRight Affects Lengths and Moves Left if at end" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.deleteRight)
        yield zn.lefts.length must be_===(z.lefts.length - 1)
    ) getOrElse ( xs.length must be_<(2) )
  }

  "deleteRight moves the focus to the right or if not possible left" ! prop { z: Zipper[Int] =>
    (
      for {
        zd <- z.deleteRight
      } yield {
        if (z.rights.length > 0) zd.focus must be_===(z.rights(0))
        else                     zd.focus must be_===(z.lefts(0))
      }
    ) getOrElse{ (z.lefts.isEmpty must beTrue) and (z.rights.isEmpty must beTrue) }
  }

  "deleteRightOr should return Some of deleteLeft or an alternative" ! prop { (z: Zipper[Int], alt: Zipper[Int]) =>
    val zd = z.deleteRightOr(alt)
    if (z.lefts.length == 0 && z.rights.length == 0)
      zd must be_===(alt)
    else if (z.rights.length != 0)
      (zd.rights must be_===(z.rights.tail)) and (zd.lefts must be_===(z.lefts))
    else
      (zd.rights.isEmpty must beTrue) and (zd.lefts must be_===(z.lefts.tail))
  }

  "DeleteLeft Affects Lengths" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.deleteLeft)
        yield zn.lefts.length must be_===(z.lefts.length - 1)
    ) getOrElse (xs.length must be_<(2))
  }

  "deleteLeft moves the focus to the left or if not possible right" ! prop { z: Zipper[Int] =>
    (
      for {
        zd <- z.deleteLeft
      } yield {
        if (z.lefts.length > 0) zd.focus must be_===(z.lefts(0))
        else                    zd.focus must be_===(z.rights(0))
      }
    ) getOrElse((z.lefts.isEmpty must beTrue) and (z.rights.isEmpty must beTrue))
  }

  "DeleteLeftC Affects Lengths" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.deleteLeftC)
        yield zn.lefts.length must be_===(z.lefts.length - 1)
    ) getOrElse (xs.length must be_<(2))
  }
 
  "deleteLeftC moves the focus to the left or if not possible to the last element" ! prop { z: Zipper[Int] =>
    (
      for {
        zd <- z.deleteLeftC
      } yield {
        if (z.lefts.length > 0) zd.focus must be_===(z.lefts(0))
        else                    zd.focus must be_===(z.rights.last)
      }
    ) getOrElse((z.lefts.isEmpty must beTrue) and (z.rights.isEmpty must beTrue))
  }

  "deleteLeftCOr should return Some of deleteLeftC or an alternative" ! prop { (z: Zipper[Int], alt: Zipper[Int]) =>
    val zd = z.deleteLeftCOr(alt)
    if (z.lefts.length == 0 && z.rights.length == 0) zd must be_===(alt)
    else z.deleteLeftC must be_===(Some(zd))
  }

  "deleteRightOr should return Some of deleteLeft or an alternative" ! prop { (z: Zipper[Int], alt: Zipper[Int]) =>
    val zd = z.deleteRightOr(alt)
    if (z.lefts.length == 0 && z.rights.length == 0) zd must be_===(alt)
    else if (z.rights.length != 0){
      zd.rights must be_===(z.rights.tail)
      zd.lefts must be_===(z.lefts)
    }else{
      zd.rights.isEmpty must beTrue
      zd.lefts must be_===(z.lefts.tail)
    }
  }

  "DeleteLeft Affects Lengths and Moves Right if at start" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.deleteLeft)
        yield zn.rights.length must be_===(z.rights.length - 1)
    ) getOrElse (xs.length must be_<(2))
  }

  "deleteLeftOr should return Some of deleteLeft or an alternative" ! prop { (z: Zipper[Int], alt: Zipper[Int]) =>
    if (z.lefts.length == 0 && z.rights.length == 0) z.deleteLeftOr(alt) must be_===(alt)
    else {
      val zd = z.deleteLeftOr(alt)
      if (z.lefts.length != 0){
        zd.lefts must be_===(z.lefts.tail)
        zd.rights must be_===(z.rights)
      }else{
        zd.lefts.isEmpty must beTrue
        zd.rights must be_===(z.rights.tail)
      }
    }
  }

  "DeleteRightC Affects Lengths and Cycles to Start if at end" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.deleteRightC)
        yield zn.rights.length must be_===(z.lefts.length - 1)
    ) getOrElse (xs.length must be_<(2))
  }

  "DeleteLeftC Affects Lengths and Cycles to end if at start" ! prop { (xs: Stream[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.deleteLeftC)
        yield zn.lefts.length must be_===(z.rights.length - 1)
    ) getOrElse (xs.length must be_<(2))
  }

  "Move" ! prop { (xs: Stream[Int], ys: Stream[Int], f: Int, n: Short) =>
    
    zipper(xs, f, ys).move(n) map { (z: Zipper[Int]) =>
      z.lefts.length must be_===(xs.length + n)
      z.rights.length must be_===(ys.length - n)
      if(n > 0)
        ys(n - 1) must be_===(z.focus)
      else if(n < 0)
        xs(-(n + 1)) must be_===(z.focus)
      else
        f must be_===(z.focus)
    } getOrElse {
      (xs.length must be_<(-n)) or (ys.length must be_<(n: Int))
    }
  }

  "move should not cause a stackoverflow error" in {
    val size = 32 * 1024 
    val n = size - 1

    val f = for {
      z <- Stream.from(1).take(size).toZipper
      zm <- z.move(n)
    } yield zm.focus
    
    f must be_===(Some(size))
  }

  "moveOr should return some of move or an alternative" ! prop { 
    (l: Stream[Int], f: Int, r: Stream[Int], n: Short, alt: Zipper[Int]) =>

    val z = zipper(l, f, r).moveOr(n, alt)
    if (l.length < (-n) || r.length < n) z must be_===(alt)
    else {
      z.lefts.length must be_===(l.length + n)
      z.rights.length must be_===(r.length - n)
      if(n > 0)
        r(n - 1) must be_===(z.focus)
      else if(n < 0)
        l(-(n + 1)) must be_===(z.focus)
      else
        f must be_===(z.focus)
    }
  }

  "Length should return the size of the zipper" ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    zipper(l, f, r).length must be_===(l.length + 1 + r.length)
  }

  "The zipper should be atStart when the lefts stream is empty" ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    if (zipper(l, f, r).atStart) l.isEmpty must beTrue
    else l.isEmpty must beFalse
  }

  "withFocus should pair only the focus with true, false otherwise" ! prop { z: Zipper[Int] =>
    val zf = z.withFocus
    zf.lefts.find(_._2).isEmpty must beTrue
    zf.focus._2 must beTrue
    zf.rights.find(_._2).isEmpty must beTrue
  }

  "start should set the zipper at the start" ! prop { z: Zipper[Int] =>
    val zs = z.start
    zs.toStream must be_===(z.toStream)
    zs.index must be_===(0)
  }

  "end should set the zipper at the end" ! prop { z: Zipper[Int] =>
    val ze = z.end
    ze.toStream must be_===(z.toStream)
    ze.index must be_===(ze.length - 1)
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
      p(z.findZor(p, alt).focus) must beTrue
    } else {
      z.findZor(p, alt) must be_===(alt)
    }
  }

  "findZ should not cause a stackoverflow error" in {
    val size = 32 * 1024
    val elem = size - 1
    val r = for {
      z <- Stream.from(1).take(size).toZipper
      zf <- z.findZ(_ == elem)
    } yield zf.focus

    r must be_===(Some(elem))
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
    zipper(xs, f, ys).update(u) must be_===(zipper(xs, u, ys))
  }

  "Modify Modifies Zipper Correctly" ! prop { (xs: Stream[Int], ys: Stream[Int], f: Int, u: Int) =>
    val modF: Int => Int = _ + u
    zipper(xs, f, ys).modify(modF) must be_===(zipper(xs, f + u, ys))
  }

  "Start" ! prop { (xs: Stream[Int], ys: Stream[Int], f: Int) =>
    val zo = zipper(xs, f, ys)
    val z = zo.start

    z.lefts.length must be_===(0)
    z.rights.length must be_===(z.length - 1)
    zo.move(-xs.length) must be_===(Some(z))
    (z.move(xs.length) must be_===(Some(zo))) or (z.length must be_===(0))
  }

  "End" ! prop { (xs: Stream[Int], ys: Stream[Int], f: Int) =>
    val zo = zipper(xs, f, ys)
    val z = zo.end

    z.lefts.length must be_===(z.length - 1)
    z.rights.length must be_===(0)
    zo.move(ys.length) must be_===(Some(z))
    (z.move(-ys.length) must be_===(Some(zo))) or (z.length must be_===(0))
  }

  "positions should return a zippers with focus on this" ! prop { z: Zipper[Int] =>
    z.positions.focus must be_===(z)
  }

  "positions should return a zippers with all possible positions of a zipper" ! prop { z: Zipper[Int] =>
    val indeces = z.positions.map { _.index }.toStream
    indeces.min must be_===(0)
    indeces.max must be_===(z.length -1)
    indeces.sorted must be_===(indeces)
    z.positions.map { _.toStream }.toStream.distinct.length must be_===(1)
  }

  "index returns the position of the focus" ! prop { (l: Stream[Int], f: Int, r: Stream[Int]) =>
    zipper(l, f, r).index must be_===(l.length)
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
