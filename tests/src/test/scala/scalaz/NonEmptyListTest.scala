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

  "NonEmptyList.init size is correct" ! prop { xs:NonEmptyList[Int] =>
    xs.init.size must be_===(xs.tail.size)
  }
}
