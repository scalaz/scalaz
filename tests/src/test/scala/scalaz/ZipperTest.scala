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

  property("Move") = forAll((xs: Stream[Int], ys: Stream[Int], f: Int, n: Short) =>
    (zipper(xs, f, ys).move(n) map ((z: Zipper[Int]) =>
      z.lefts.length === xs.length + n &&
              z.rights.length === ys.length - n &&
              (n > 0 && ys(n - 1) === z.focus || n < 0 && xs(-(n + 1)) === z.focus || f === z.focus))).
            getOrElse(xs.length < (-n) || ys.length < n)
    )

  property("Find") = forAll((xs: Stream[Int], ys: Stream[Int], f: Int, n: Int, m: Int) => {
    val p = (i: Int) => i < n && i > m
    ((zipper(xs, f, ys).findZ(p)) map (z =>
      p(z.focus)) getOrElse !(xs.find(p).isDefined || ys.find(p).isDefined || p(f)))
  })
}

