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
  val props = new Properties("Zipper") {
    property("Zipper From Stream") = forAll((xs: Stream[Int]) =>
      (xs.toZipper map (_.toStream)).getOrElse(Stream()) === xs)

    property("Zipper Move Then To Stream") = forAll(nels(1, 2, 3, 4)) {
      (n: NonEmptyList[Int]) =>
        n.toZipper.move(2).map(_.toStream).exists(_ === n.stream)
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


  checkAll("Zipper", props)
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

    checkAll("Zipper", traverse.laws[Zipper])
    checkAll("Zipper", applicative.laws[Zipper])
    checkAll("Zipper", comonad.laws[Zipper])
  }
}
