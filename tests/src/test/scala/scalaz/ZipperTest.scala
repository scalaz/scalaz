package scalaz

import Scalaz._
import Zipper._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import scalaz.Maybe.just

object ZipperTest extends SpecLite {

  "Zipper From LazyList" ! forAll { (xs: LazyList[Int]) =>
    (xs.toZipper map (_.toLazyList)).getOrElse(LazyList()) === xs
  }

  "Mapping on a zipper should be the same as mapping on the elements of the stream" ! forAll { (xs: LazyList[Int], a: Int) =>
    val fun: Int => Int = _ + a

    (
      for (z <- xs.toZipper; zM <- (xs map fun).toZipper) yield z.map(fun) must_===(zM)
    ) getOrElse { xs.length must_===(0) }
  }

  "Zipper Move Then To LazyList" in check {
    val n = NonEmptyList(1, 2, 3, 4)
    n.toZipper.move(2).map(_.toLazyList).exists(_ ==(n.stream))
  }

  "Next Affects Lengths" ! forAll { (xs: LazyList[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.next)
        yield {
        (zn.lefts.length must_===(z.lefts.length + 1));
        (zn.rights.length must_===(z.rights.length - 1))
      }
    ) getOrElse { xs.length mustBe_<(2) }
  }

  "nextC moves focus or loops" ! forAll { (z: Zipper[Int]) =>
    val zn = z.nextC
    zn.toLazyList must_===(z.toLazyList)

    if (z.atEnd) zn.atStart must_==(true)
    else zn.index must_===(z.index + 1)
  }

  "Next changes focus, lefts and rights " ! forAll { (l: LazyList[Int], f: Int, r: LazyList[Int]) =>
    if (r.length > 0) {
      val nextZipper = zipper(l, f, r).next.toOption.get
      nextZipper.focus must_===(r(0))
      nextZipper.lefts must_===(f +: l)
      nextZipper.rights must_===(r.tail)
    } else {
      zipper(l, f, r).next.isEmpty must_==(true)
    }
  }

  "Zipper next returns Some when rights is nonempty, none otherwise." ! forAll { (l: LazyList[Int], f: Int, r: LazyList[Int]) =>
    if (r.length > 0) zipper(l, f, r).next.isJust must_==(true)
    else zipper(l, f, r).next.isJust must_==(false)
  }

  "Zipper nextOr returns a new zipper when used on empty rights or Some of next" ! forAll {
    (l: LazyList[Int], f: Int, r: LazyList[Int], alt: Zipper[Int]) =>

    val z = zipper(l, f, r)
    if (r.length > 0) {
      z.next must_=== just(z.nextOr(alt))
    } else {
      z.nextOr(alt) must_===(alt)
    }
  }

  "Previous Affects Lengths" ! forAll { (xs: LazyList[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.previous)
        yield zn.lefts.length must_===(z.lefts.length - 1) and (zn.rights.length must_===(z.rights.length + 1))
    ) getOrElse { xs.length mustBe_<(2) }
  }

  "previousC moves focus or loops" ! forAll { (z: Zipper[Int]) =>
    val zp = z.previousC
    zp.toLazyList must_===(z.toLazyList)

    if (z.atStart) zp.atEnd must_==(true)
    else zp.index must_===(z.index - 1)
  }

  "Previous changes the focus, lefts and rights " ! forAll { (l: LazyList[Int], f: Int, r: LazyList[Int]) =>
    if (l.length > 0) {
      val prevZipper = zipper(l, f, r).previous.toOption.get
      prevZipper.focus must_===(l(0))
      prevZipper.lefts must_===(l.tail)
      prevZipper.rights must_===(f +: r)
    } else {
      zipper(l, f, r).previous.isJust must_== false
    }
  }

  "Zipper previousOr returns a new zipper when used on empty rights or Some of next" ! forAll {
    (l: LazyList[Int], f: Int, r: LazyList[Int], alt: Zipper[Int]) =>

    val z = zipper(l, f, r)
    if (l.length > 0) {
      z.previous must_=== just(z.previousOr(alt))
    } else {
      z.previousOr(alt) must_===(alt)
    }
  }

  "Zipper tryPrevious returns Some of next or throws" ! forAll { (l: LazyList[Int], f: Int, r: LazyList[Int]) =>
    val z = zipper(l, f, r)
    if (l.length > 0) {
      z.previous must_=== just(z.tryPrevious)
    } else {
      z.tryPrevious.mustThrowA[RuntimeException]
    }
  }

  def insertionTest(
      name: String,
      insertion: (Zipper[Int], Int) => Zipper[Int],
      pred: (Zipper[Int], Zipper[Int], Int) => Prop) = name ! forAll { (z: Zipper[Int], e: Int) =>

    val zi = insertion(z, e)
    pred(zi, z, e)
  }

  val leftAndFocusChanged: (Zipper[Int], Zipper[Int], Int) => Prop = { (zNew, zOld, newFocus) =>
    {zNew.focus must_===(newFocus)} and
    {zNew.lefts.head  must_===(zOld.focus)} and
    {zNew.lefts.tail must_===(zOld.lefts)} and
    {zNew.rights must_===(zOld.rights)}
  }

  val rightAndFocusChanged: (Zipper[Int], Zipper[Int], Int) => Prop = { (zNew, zOld, newFocus) =>
    {zNew.focus must_===(newFocus)} and
    {zNew.lefts must_===(zOld.lefts)} and
    {zNew.rights.head must_===(zOld.focus)} and
    {zNew.rights.tail must_===(zOld.rights)}
  }

  insertionTest("insertRight changes focus and appends to lefts", (z, e) => z.insertRight(e), leftAndFocusChanged)
  insertionTest("insert changes focus and appends to lefts",      (z, e) => z.insert(e),      leftAndFocusChanged)
  insertionTest("insertLeft changes focus and appends to lefts",  (z, e) => z.insertLeft(e),  rightAndFocusChanged)

  "DeleteRight Affects Lengths" ! forAll { (xs: LazyList[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.deleteRight)
        yield zn.rights.length must_===(z.rights.length - 1)
    ) getOrElse {xs.length mustBe_<(2) }
  }

  "DeleteRightC Affects Lengths" ! forAll { (xs: LazyList[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.deleteRightC)
        yield zn.rights.length must_===(z.rights.length - 1)
    ) getOrElse {xs.length mustBe_<(2) }
  }

  "deleteRightC moves the focus to the right or if not possible to the first element" ! forAll { (z: Zipper[Int]) =>
    (
      for {
        zd <- z.deleteRightC
      } yield {
        if (z.rights.length > 0) zd.focus must_===(z.rights(0))
        else                     zd.focus must_===(z.lefts.last)
      }
    ) getOrElse{ (z.lefts.isEmpty must_==(true)) and (z.rights.isEmpty must_==(true)) }
  }

  "deleteRightCOr should return Some of deleteLeftC or an alternative" ! forAll { (z: Zipper[Int], alt: Zipper[Int]) =>
    val zd = z.deleteRightCOr(alt)
    if (z.lefts.length == 0 && z.rights.length == 0) zd must_===(alt)
    else z.deleteRightC must_=== just(zd)
  }

  "DeleteRight Affects Lengths and Moves Left if at end" ! forAll { (xs: LazyList[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.deleteRight)
        yield zn.lefts.length must_===(z.lefts.length - 1)
    ) getOrElse ( xs.length mustBe_<(2) )
  }

  "deleteRight moves the focus to the right or if not possible left" ! forAll { (z: Zipper[Int]) =>
    (
      for {
        zd <- z.deleteRight
      } yield {
        if (z.rights.length > 0) zd.focus must_===(z.rights(0))
        else                     zd.focus must_===(z.lefts(0))
      }
    ) getOrElse{ (z.lefts.isEmpty must_==(true)) and (z.rights.isEmpty must_==(true)) }
  }

  "deleteRightOr should return Some of deleteLeft or an alternative" ! forAll { (z: Zipper[Int], alt: Zipper[Int]) =>
    val zd = z.deleteRightOr(alt)
    if (z.lefts.length == 0 && z.rights.length == 0)
      zd must_===(alt)
    else if (z.rights.length != 0)
      (zd.rights must_===(z.rights.tail)) and (zd.lefts must_===(z.lefts))
    else
      (zd.rights.isEmpty must_==(true)) and (zd.lefts must_===(z.lefts.tail))
  }

  "DeleteLeft Affects Lengths" ! forAll { (xs: LazyList[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.deleteLeft)
        yield zn.lefts.length must_===(z.lefts.length - 1)
    ) getOrElse (xs.length mustBe_<(2))
  }

  "deleteLeft moves the focus to the left or if not possible right" ! forAll { (z: Zipper[Int]) =>
    (
      for {
        zd <- z.deleteLeft
      } yield {
        if (z.lefts.length > 0) zd.focus must_===(z.lefts(0))
        else                    zd.focus must_===(z.rights(0))
      }
    ) getOrElse((z.lefts.isEmpty must_==(true)) and (z.rights.isEmpty must_==(true)))
  }

  "DeleteLeftC Affects Lengths" ! forAll { (xs: LazyList[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.deleteLeftC)
        yield zn.lefts.length must_===(z.lefts.length - 1)
    ) getOrElse (xs.length mustBe_<(2))
  }

  "deleteLeftC moves the focus to the left or if not possible to the last element" ! forAll { (z: Zipper[Int]) =>
    (
      for {
        zd <- z.deleteLeftC
      } yield {
        if (z.lefts.length > 0) zd.focus must_===(z.lefts(0))
        else                    zd.focus must_===(z.rights.last)
      }
    ) getOrElse((z.lefts.isEmpty must_==(true)) and (z.rights.isEmpty must_==(true)))
  }

  "deleteLeftCOr should return Some of deleteLeftC or an alternative" ! forAll { (z: Zipper[Int], alt: Zipper[Int]) =>
    val zd = z.deleteLeftCOr(alt)
    if (z.lefts.length == 0 && z.rights.length == 0) zd must_===(alt)
    else z.deleteLeftC must_=== just(zd)
  }

  "deleteRightOr should return Some of deleteLeft or an alternative" ! forAll { (z: Zipper[Int], alt: Zipper[Int]) =>
    val zd = z.deleteRightOr(alt)
    if (z.lefts.length == 0 && z.rights.length == 0) zd must_===(alt)
    else if (z.rights.length != 0){
      zd.rights must_===(z.rights.tail)
      zd.lefts must_===(z.lefts)
    }else{
      zd.rights.isEmpty must_==(true)
      zd.lefts must_===(z.lefts.tail)
    }
  }

  "DeleteLeft Affects Lengths and Moves Right if at start" ! forAll { (xs: LazyList[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.deleteLeft)
        yield zn.rights.length must_===(z.rights.length - 1)
    ) getOrElse (xs.length mustBe_<(2))
  }

  "deleteLeftOr should return Some of deleteLeft or an alternative" ! forAll { (z: Zipper[Int], alt: Zipper[Int]) =>
    if (z.lefts.length == 0 && z.rights.length == 0) z.deleteLeftOr(alt) must_===(alt)
    else {
      val zd = z.deleteLeftOr(alt)
      if (z.lefts.length != 0){
        zd.lefts must_===(z.lefts.tail)
        zd.rights must_===(z.rights)
      }else{
        zd.lefts.isEmpty must_==(true)
        zd.rights must_===(z.rights.tail)
      }
    }
  }

  "DeleteRightC Affects Lengths and Cycles to Start if at end" ! forAll { (xs: LazyList[Int]) =>
    (
      for (z <- xs.zipperEnd; zn <- z.deleteRightC)
        yield zn.rights.length must_===(z.lefts.length - 1)
    ) getOrElse (xs.length mustBe_<(2))
  }

  "DeleteLeftC Affects Lengths and Cycles to end if at start" ! forAll { (xs: LazyList[Int]) =>
    (
      for (z <- xs.toZipper; zn <- z.deleteLeftC)
        yield zn.lefts.length must_===(z.rights.length - 1)
    ) getOrElse (xs.length mustBe_<(2))
  }

  "Move" ! forAll { (xs: LazyList[Int], ys: LazyList[Int], f: Int, n: Short) =>

    zipper(xs, f, ys).move(n) map { (z: Zipper[Int]) =>
      z.lefts.length must_===(xs.length + n)
      z.rights.length must_===(ys.length - n)
      if(n > 0)
        ys(n - 1) must_===(z.focus)
      else if(n < 0)
        xs(-(n + 1)) must_===(z.focus)
      else
        f must_===(z.focus)
    } getOrElse {
      val okay = xs.length < -n || ys.length < n
      okay must_==(true)
    }
  }

  "move should not cause a stackoverflow error" in {
    val size = 32 * 1024
    val n = size - 1

    val f = for {
      z <- LazyList.from(1).take(size).toZipper
      zm <- z.move(n)
    } yield zm.focus

    f must_=== just(size)
  }

  "moveOr should return some of move or an alternative" ! forAll {
    (l: LazyList[Int], f: Int, r: LazyList[Int], n: Short, alt: Zipper[Int]) =>

    val z = zipper(l, f, r).moveOr(n, alt)
    if (l.length < (-n) || r.length < n) z must_===(alt)
    else {
      z.lefts.length must_===(l.length + n)
      z.rights.length must_===(r.length - n)
      if(n > 0)
        r(n - 1) must_===(z.focus)
      else if(n < 0)
        l(-(n + 1)) must_===(z.focus)
      else
        f must_===(z.focus)
    }
  }

  "Length should return the size of the zipper" ! forAll { (l: LazyList[Int], f: Int, r: LazyList[Int]) =>
    zipper(l, f, r).length must_===(l.length + 1 + r.length)
  }

  "The zipper should be atStart when the lefts stream is empty" ! forAll { (l: LazyList[Int], f: Int, r: LazyList[Int]) =>
    if (zipper(l, f, r).atStart) l.isEmpty must_==(true)
    else l.isEmpty must_==(false)
  }

  "withFocus should pair only the focus with true, false otherwise" ! forAll { (z: Zipper[Int]) =>
    val zf = z.withFocus
    zf.lefts.find(_._2).isEmpty must_==(true)
    zf.focus._2 must_==(true)
    zf.rights.find(_._2).isEmpty must_==(true)
  }

  "start should set the zipper at the start" ! forAll { (z: Zipper[Int]) =>
    val zs = z.start
    zs.toLazyList must_===(z.toLazyList)
    zs.index must_===(0)
  }

  "end should set the zipper at the end" ! forAll { (z: Zipper[Int]) =>
    val ze = z.end
    ze.toLazyList must_===(z.toLazyList)
    ze.index must_===(ze.length - 1)
  }

  "The zipper should be atEnd when the right stream is empty" ! forAll { (l: LazyList[Int], f: Int, r: LazyList[Int]) =>
    if (zipper(l, f, r).atEnd) r.isEmpty
    else !r.isEmpty
  }

  "Find" ! forAll { (xs: LazyList[Int], ys: LazyList[Int], f: Int, n: Int, m: Int) =>
    val p = (i: Int) => i < n && i > m
    zipper(xs, f, ys).findZ(p) map { z => p(z.focus) } getOrElse !(xs.find(p).isDefined || ys.find(p).isDefined || p(f))
  }

  "findZ shouldn't change elements" ! forAll { (xs: LazyList[Int], ys: LazyList[Int], f: Int, n: Int, m: Int) =>
    val p = (i: Int) => i < n && i > m
    zipper(xs, f, ys).findZ(p).map {
      _.toLazyList == zipper(xs, f, ys).toLazyList
    } getOrElse !(xs.find(p).isDefined || ys.find(p).isDefined || p(f))
  }

  "findZor returns some of find or an alternative" ! forAll {
    (z: Zipper[Int], n: Int, m: Int, alt: Zipper[Int]) =>
    val p = (i: Int) => i < n && i > m

    if (z.lefts.find(p).isDefined || p(z.focus) || z.rights.find(p).isDefined) {
      p(z.findZor(p, alt).focus) must_==(true)
    } else {
      z.findZor(p, alt) must_===(alt)
    }
  }

  "findZ should not cause a stackoverflow error" in {
    val size = 32 * 1024
    val elem = size - 1
    val r = for {
      z <- LazyList.from(1).take(size).toZipper
      zf <- z.findZ(_ == elem)
    } yield zf.focus

    r must_=== just(elem)
  }

  "findBy if given a function that returns None should not return anything" ! forAll { (z: Zipper[Int]) =>
    z.findBy(z => Maybe.empty)(x => x == z.focus).isEmpty
  }

  val intZipperWithExistingElement: Gen[(Zipper[Int], Int)] = for {
    z <- arbitrary[Zipper[Int]]
    stream = z.toLazyList
    i <- Gen.choose(0, stream.length -1)
  } yield (z, stream(i))

  "given nextC findBy should return Some if the element exists" !  forAll(intZipperWithExistingElement) { case (z, e) =>
    z.findBy(z => just(z.nextC))(x => x == e).isDefined
   }

  "findBy should not blow the stack" !  prop { (z: Zipper[Int]) =>
    var limit = 10 * 1000
    z.findBy(z => if (limit > 0) { limit -= 1; just(z.nextC) } else Maybe.empty)(x => false)
    true
   }

  def minSizeIntZipper(size: Int): Gen[Zipper[Int]] = for {
      leftSize <- Gen.choose(0, size - 2)
      rightSize = size - 1 - leftSize
      lefts  <- Gen.containerOfN[LazyList,Int](leftSize,  implicitly[Arbitrary[Int]].arbitrary)
      rights <- Gen.containerOfN[LazyList,Int](rightSize, implicitly[Arbitrary[Int]].arbitrary)
      focus <- arbitrary[Int]
  } yield zipper(lefts, focus, rights)

  "findNext should not blow the stack" !  forAll(minSizeIntZipper(10 * 1000)) { z =>
    var limit = 10 * 1000
    z.start.findNext { x => limit -= 1; limit > 0 }
    true
   }

  "findPrevious should not blow the stack" !  forAll(minSizeIntZipper(10 * 1000)) { z =>
    var limit = 10 * 1000
    z.end.findPrevious { x => limit -= 1; limit > 0 }
    true
   }

  "Update Modifies Zipper Correctly" ! forAll { (xs: LazyList[Int], ys: LazyList[Int], f: Int, u: Int) =>
    zipper(xs, f, ys).update(u) must_===(zipper(xs, u, ys))
  }

  "Modify Modifies Zipper Correctly" ! forAll { (xs: LazyList[Int], ys: LazyList[Int], f: Int, u: Int) =>
    val modF: Int => Int = _ + u
    zipper(xs, f, ys).modify(modF) must_===(zipper(xs, f + u, ys))
  }

  "Start" ! forAll { (xs: LazyList[Int], ys: LazyList[Int], f: Int) =>
    val zo = zipper(xs, f, ys)
    val z = zo.start

    z.lefts.length must_=== 0
    z.rights.length must_===(z.length - 1)
    zo.move(-xs.length) must_=== just(z)
    ((z.move(xs.length) == just(zo)) || z.length == 0) must_== true
  }

  "End" ! forAll { (xs: LazyList[Int], ys: LazyList[Int], f: Int) =>
    val zo = zipper(xs, f, ys)
    val z = zo.end

    z.lefts.length must_===(z.length - 1)
    z.rights.length must_=== 0
    zo.move(ys.length) must_=== just(z)
    (z.move(-ys.length) == just(zo) || (z.length == 0)) must_== true
  }

  "positions should return a zippers with focus on this" ! forAll { (z: Zipper[Int]) =>
    z.positions.focus must_===(z)
  }

  "positions should return a zippers with all possible positions of a zipper" ! forAll { (z: Zipper[Int]) =>
    val indices = z.positions.map { _.index }.toLazyList
    indices.min must_===(0)
    indices.max must_===(z.length -1)
    indices.sorted must_===(indices)
    z.positions.map { _.toLazyList }.toLazyList.distinct.length must_===(1)
  }

  "index returns the position of the focus" ! forAll { (l: LazyList[Int], f: Int, r: LazyList[Int]) =>
    zipper(l, f, r).index must_===(l.length)
  }

  checkAll("Zipper", equal.laws[Zipper[Int]])
  checkAll("Zipper", traverse1.laws[Zipper])
  checkAll("Zipper", FoldableTests.anyAndAllLazy[Zipper])
  checkAll("Zipper", comonad.laws[Zipper])

  {
    implicit def zipperEqual[A: Equal]: Equal[Zipper[A]] = new Equal[Zipper[A]] {
      import std.lazylist._
      def equalApprox = Equal[LazyList[A]].contramap((_: LazyList[A]).take(1000))
      def equal(a1: Zipper[A], a2: Zipper[A]) =
        equalApprox.equal(a1.lefts, a2.lefts) &&
          Equal[A].equal(a1.focus, a2.focus) &&
          equalApprox.equal(a1.rights, a2.rights)
    }

    checkAll("Zipper", applicative.laws[Zipper])
  }
}

// vim: expandtab:ts=2:sw=2
