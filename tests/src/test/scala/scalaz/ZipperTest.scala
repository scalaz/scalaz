package scalaz

import Scalaz._
import org.scalacheck._
import org.scalacheck.Prop._

object ZipperSpec extends Properties("Zipper") {
  property("Zipper From Stream") = forAll((xs: Stream[Int]) =>
    (xs.toZipper map (_.toStream)).getOrElse(Stream()) === xs)

  property("Zipper Move Then To Stream") = forAll(nel(1, 2, 3, 4)) {(n: NonEmptyList[Int]) =>
    n.toZipper.move(2).map(_.toStream).element(n.stream)
  }

  property("Next Affects Lengths") = forAll((xs: Stream[Int]) => {
    (for (z <- xs.toZipper;
          zn <- z.next)
    yield zn.lefts.length === z.lefts.length + 1 &&
              zn.rights.length === z.rights.length - 1) getOrElse (xs.length < 2)
  })

  property("Previous Affects Lengths") = forAll((xs: Stream[Int]) => {
    (for (z <- xs.zipperEnd;
          zn <- z.previous)
    yield zn.lefts.length === z.lefts.length - 1 &&
              zn.rights.length === z.rights.length + 1) getOrElse (xs.length < 2)
  })

  property("DeleteRight Affects Lengths") = forAll((xs: Stream[Int]) => {
    (for (z <- xs.toZipper;
          zn <- z.deleteRight)
    yield zn.rights.length === z.rights.length - 1) getOrElse (xs.length < 2)
  })
  property("DeleteRightC Affects Lengths") = forAll((xs: Stream[Int]) => {
    (for (z <- xs.toZipper;
          zn <- z.deleteRightC)
    yield zn.rights.length === z.rights.length - 1) getOrElse (xs.length < 2)
  })
  property("DeleteRight Affects Lengths and Moves Left if at end") = forAll((xs: Stream[Int]) => {
    (for (z <- xs.zipperEnd;
          zn <- z.deleteRight)
    yield zn.lefts.length === z.lefts.length - 1) getOrElse (xs.length < 2)
  })
  property("DeleteLeft Affects Lengths") = forAll((xs: Stream[Int]) => {
    (for (z <- xs.zipperEnd;
          zn <- z.deleteLeft)
    yield zn.lefts.length === z.lefts.length - 1) getOrElse (xs.length < 2)
  })
  property("DeleteLeftC Affects Lengths") = forAll((xs: Stream[Int]) => {
    (for (z <- xs.zipperEnd;
          zn <- z.deleteLeftC)
    yield zn.lefts.length === z.lefts.length - 1) getOrElse (xs.length < 2)
  })
  property("DeleteLeft Affects Lengths and Moves Right if at start") = forAll((xs: Stream[Int]) => {
    (for (z <- xs.toZipper;
          zn <- z.deleteLeft)
    yield zn.rights.length === z.rights.length - 1) getOrElse (xs.length < 2)
  })

  property("DeleteRightC Affects Lengths and Cycles to Start if at end") = forAll((xs: Stream[Int]) => {
    (for (z <- xs.zipperEnd;
          zn <- z.deleteRightC)
    yield zn.rights.length === z.lefts.length - 1) getOrElse (xs.length < 2)
  })
  property("DeleteLeftC Affects Lengths and Cycles to end if at start") = forAll((xs: Stream[Int]) => {
    (for (z <- xs.toZipper;
          zn <- z.deleteLeftC)
    yield zn.lefts.length === z.rights.length - 1) getOrElse (xs.length < 2)
  })

  property("Move") = forAll((xs: Stream[Int], ys: Stream[Int], f: Int, n: Short) =>
    (zipper(xs, f, ys).move(n) map ((z: Zipper[Int]) =>
      z.lefts.length === xs.length + n &&
              z.rights.length === ys.length - n &&
              (n > 0 && ys(n - 1) === z.focus || n < 0 && xs(-(n + 1)) === z.focus || f === z.focus))).
            getOrElse(xs.length < (-n) || ys.length < n)
    )

  property("Update Modifies Zipper Correctly") = forAll((xs: Stream[Int], ys: Stream[Int], f: Int, u: Int) =>
    zipper(xs, f, ys).update(u) === zipper(xs, u, ys)
  )

  property("Start") = forAll((xs: Stream[Int], ys: Stream[Int], f: Int) => {
    val zo = zipper(xs, f, ys)
    val z = zo.start
    z.lefts.length === 0 &&
        z.rights.length === z.length - 1 &&
        z === zo.move(-xs.length).get &&
        z.move(xs.length).map(_ === zo).getOrElse(false)
  })
  property("End") = forAll((xs: Stream[Int], ys: Stream[Int], f: Int) => {
    val zo = zipper(xs, f, ys)
    val z = zo.end
    z.lefts.length === z.length - 1 &&
        z.rights.length === 0 &&
        z === zo.move(ys.length).get &&
        z.move(-ys.length).map(_ === zo).getOrElse(z.length == 0)
  })

  property("Find") = forAll((xs: Stream[Int], ys: Stream[Int], f: Int, n: Int, m: Int) => {
    val p = (i: Int) => i < n && i > m
    ((zipper(xs, f, ys).findZ(p)) map (z =>
      p(z.focus)) getOrElse !(xs.find(p).isDefined || ys.find(p).isDefined || p(f)))
  })
}

