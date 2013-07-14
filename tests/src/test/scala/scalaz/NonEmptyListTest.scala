package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

class NonEmptyListTest extends Spec {
  checkAll("NonEmptyList", monad.laws[NonEmptyList])
  checkAll("NonEmptyList", plus.laws[NonEmptyList])
  checkAll("NonEmptyList", semigroup.laws[NonEmptyList[Int]])
  checkAll("NonEmptyList", equal.laws[NonEmptyList[Int]])
  checkAll("NonEmptyList", order.laws[NonEmptyList[Int]])
  checkAll("NonEmptyList", traverse.laws[NonEmptyList])
  checkAll("NonEmptyList", comonad.laws[NonEmptyList])

  "NonEmptyList size is correct" ! prop { xs:NonEmptyList[Int] =>
    xs.size must be_===(1 + xs.tail.size) 
  }

  "foldl1 is reduceLeft" ! prop {(rnge: NonEmptyList[List[Int]]) =>
    val F = Foldable1[NonEmptyList]
    rnge.list.reduceLeft(_++_) must be_===(F.foldl1(rnge)(a => b => a ++ b))
  }

  "foldr1 is reduceRight" ! prop {(rnge: NonEmptyList[List[Int]]) =>
    val F = Foldable1[NonEmptyList]
    rnge.list.reduceRight(_++_) must be_===(F.foldr1(rnge)(a => b => a ++ b))
  }
  "foldRight1 is reduceRight" ! prop { xs: NonEmptyList[List[Int]] =>
    val F = Foldable1[NonEmptyList]
    xs.list.reduceRight(_ ++ _) must_== F.foldRight1(xs)(_ ++ _)
  }
  "NonEmptyList.last is correct" ! prop { xs:NonEmptyList[Int] =>
    xs.reverse.head must be_===(xs.last)
  }
  "NonEmptyList.init size is correct" ! prop { xs:NonEmptyList[Int] =>
    xs.init.size must be_===(xs.tail.size)
  }
  "NonEmptyList.foldRight1 large list" in {
    import NonEmptyList._
    import syntax.foldable1._
    nel(0, List.fill(10000000)(1)).foldRight1(_ + _) must_== 10000000
  }
}
