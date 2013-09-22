package scalaz

import std.AllInstances._
import org.specs2.matcher.Matchers.not
import org.specs2.matcher.OptionMatchers.{beSome, beNone}

class AlignTest extends Spec {

  val F = Align[List]

  "pad" ! prop{ (xs: List[Int], ys: List[Int]) =>
    val xys = F.pad(xs, ys)
    forall(xys){case (x, y) => ((x must beNone) and (y must beNone)).not}
    val max = xs.size max ys.size
    val min = xs.size min ys.size
    xys.length must be_===(max)
    xys.takeWhile{case (x, y) => x.isDefined && y.isDefined}.size must be_===(min)
    val dropped = xys.dropWhile{case (x, y) => x.isDefined && y.isDefined}
    if(xs.size > ys.size)
      forall(dropped){case (x, y) => (x must beSome) and (y must beNone)}
    else
      forall(dropped){case (x, y) => (x must beNone) and (y must beSome)}
  }

  "merge" ! prop{ (xs: List[Int], ys: List[Int]) =>
    val xys = F.merge(xs, ys)
    xys.size must be_===(xs.size max ys.size)
    xys must be_===(xs.zipAll(ys, 0, 0).map{case (x, y) => x + y})
  }

  "alignA" ! prop{ (xs: List[Int], ys: List[Int]) =>
    val xys = F.alignA(xs, ys)
    xys.size must be_===(xs.size max ys.size)
    xys.takeWhile(_.isDefined).size must be_===(xs.size)
    forall(xys.dropWhile(_.isDefined))(_ must beNone)
  }

  "alignB" ! prop{ (xs: List[Int], ys: List[Int]) =>
    val xys = F.alignB(xs, ys)
    xys.size must be_===(xs.size max ys.size)
    xys.takeWhile(_.isDefined).size must be_===(ys.size)
    forall(xys.dropWhile(_.isDefined))(_ must beNone)
  }

  "alignThis" ! prop{ (xs: List[Int], ys: List[Int]) =>
    val xys = F.alignThis(xs, ys)
    xys.size must be_===(xs.size max ys.size)
    xys.dropWhile(_.isEmpty).size must be_===((xs.size - ys.size) max 0)
    forall(xys.dropWhile(_.isEmpty))(_ must beSome)
  }

  "alignThat" ! prop{ (xs: List[Int], ys: List[Int]) =>
    val xys = F.alignThat(xs, ys)
    xys.size must be_===(xs.size max ys.size)
    xys.dropWhile(_.isEmpty).size must be_===((ys.size - xs.size) max 0)
    forall(xys.dropWhile(_.isEmpty))(_ must beSome)
  }

  "alignBoth" ! prop{ (xs: List[Int], ys: List[Int]) =>
    val xys = F.alignBoth(xs, ys)
    xys.size must be_===(xs.size max ys.size)
    xys.takeWhile(_.isDefined).size must be_===(xs.size min ys.size)
    forall(xys.dropWhile(_.isDefined))(_ must beNone)
  }

}
