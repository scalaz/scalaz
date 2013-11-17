package scalaz

import std.AllInstances._
import org.scalacheck.Prop.forAll

object AlignTest extends SpecLite {

  val F = Align[List]

  "pad" ! forAll{ (xs: List[Int], ys: List[Int]) =>
    val xys = F.pad(xs, ys)
    xys.foreach(_. mustMatch {case (None, None) => false; case _ => true})
    val max = xs.size max ys.size
    val min = xs.size min ys.size
    xys.length must_===(max)
    xys.takeWhile{case (x, y) => x.isDefined && y.isDefined}.size must_===(min)
    val dropped: List[(Option[Int], Option[Int])] = xys.dropWhile{case (x, y) => x.isDefined && y.isDefined}
    if(xs.size > ys.size)
      dropped.foreach(_ mustMatch {case (Some(_), None) => true})
    else
      dropped.foreach(_ mustMatch {case (None, Some(_)) => true})
  }

  "merge" ! forAll{ (xs: List[Int], ys: List[Int]) =>
    val xys = F.merge(xs, ys)
    xys.size must_===(xs.size max ys.size)
    xys must_===(xs.zipAll(ys, 0, 0).map{case (x, y) => x + y})
  }

  "alignA" ! forAll{ (xs: List[Int], ys: List[Int]) =>
    val xys = F.alignA(xs, ys)
    xys.size must_===(xs.size max ys.size)
    xys.takeWhile(_.isDefined).size must_===(xs.size)
    (xys.dropWhile(_.isDefined)).foreach(_ must_== None)
  }

  "alignB" ! forAll{ (xs: List[Int], ys: List[Int]) =>
    val xys = F.alignB(xs, ys)
    xys.size must_===(xs.size max ys.size)
    xys.takeWhile(_.isDefined).size must_===(ys.size)
    xys.dropWhile(_.isDefined).foreach(_ must_== None)
  }

  "alignThis" ! forAll{ (xs: List[Int], ys: List[Int]) =>
    val xys = F.alignThis(xs, ys)
    xys.size must_===(xs.size max ys.size)
    xys.dropWhile(_.isEmpty).size must_===((xs.size - ys.size) max 0)
    xys.dropWhile(_.isEmpty) foreach (_ mustMatch { case Some(_) => true })
  }

  "alignThat" ! forAll{ (xs: List[Int], ys: List[Int]) =>
    val xys = F.alignThat(xs, ys)
    xys.size must_===(xs.size max ys.size)
    xys.dropWhile(_.isEmpty).size must_===((ys.size - xs.size) max 0)
    xys.dropWhile(_.isEmpty) foreach (_ mustMatch { case Some(_) => true })
  }

  "alignBoth" ! forAll{ (xs: List[Int], ys: List[Int]) =>
    val xys = F.alignBoth(xs, ys)
    xys.size must_===(xs.size max ys.size)
    xys.takeWhile(_.isDefined).size must_===(xs.size min ys.size)
    xys.dropWhile(_.isDefined) foreach (_ must_== None)
  }

}
