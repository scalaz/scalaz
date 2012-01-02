package scalaz

import std.AllInstances._

class MonoidTest extends Spec {
  "endo multiply" in {
    import syntax.monoid._

    def inc(i: Int) = i + 1

    val incTimesThree: Endo[Int] = Endo(inc).multiply(3)
    incTimesThree(0) must be_===(3)
  }

  "unfold" in {
    val ss = Monoid.unfold[List, Int, String](1) {
      case x if x < 10 => Some((x.toString, x * 2))
      case _           => None
    }
    ss must be_===(List("1", "2", "4", "8"))
  }

  "unfold (syntax)" in {
    import syntax.monoid._

    val ss = 1.unfold[List] {
      case x if x < 10 => Some((x.toString, x * 2))
      case _           => None
    }
    ss must be_===(List("1", "2", "4", "8"))
  }

  "replicate" in {
    import syntax.monoid._

    1.replicate[Stream](3) must be_===(Stream(1, 1, 1))
    1.replicate[List](3, _ + 1) must be_===(List(1, 2, 3))
  }
}