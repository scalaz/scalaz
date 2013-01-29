package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import scalaz.scalacheck.ScalaCheckBinding._
import org.scalacheck.{Arbitrary, Gen}


class BKTreeTest extends Spec {
  implicit val IntArb = Arbitrary[Int](Gen.choose(Int.MinValue / 4, Int.MaxValue / 4))

  "string distance" in {
    BKTree[String]("kitten").containsApproximate("sitting", 3)
  }

  "empty" ! prop {
    (a: String) => !BKTree[String]().contains(a)
  }

  "singleton" ! prop {
    (a: String) => BKTree[String](a).contains(a)
  }

  "contains" ! prop {
    (a: String, as: Set[String]) => BKTree[String](as.toSeq: _*).contains(a) == as.contains(a)
  }

  "values" ! prop {
    (as: Set[String]) => BKTree[String](as.toSeq: _*).values.toSet == as
  }

  "+ and -" ! prop {
    (a:String, as: Set[String]) => (BKTree[String](as.toSeq: _*) + a - a).values.toSet == as
  }

  "isEmpty" ! prop {
    (as: Set[String]) => BKTree[String](as.toSeq: _*).isEmpty == as.isEmpty
  }

  // TODO more tests

  checkAll(functor.laws[BKTree])
  checkAll(monoid.laws[BKTree[Int]])
}
