package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

object NonEmptyListTest extends SpecLite {
  checkAll("NonEmptyList", monad.laws[NonEmptyList])
  checkAll("NonEmptyList", plus.laws[NonEmptyList])
  checkAll("NonEmptyList", semigroup.laws[NonEmptyList[Int]])
  checkAll("NonEmptyList", equal.laws[NonEmptyList[Int]])
  checkAll("NonEmptyList", order.laws[NonEmptyList[Int]])
  checkAll("NonEmptyList", traverse1.laws[NonEmptyList])
  checkAll("NonEmptyList", FoldableTests.anyAndAllLazy[NonEmptyList])
  checkAll("NonEmptyList", zip.laws[NonEmptyList])
  checkAll("NonEmptyList", align.laws[NonEmptyList])
  checkAll("NonEmptyList", comonad.laws[NonEmptyList])

  "NonEmptyList size is correct" ! forAll { xs:NonEmptyList[Int] =>
    xs.size must_===(1 + xs.tail.size) 
  }

  "foldl1 is reduceLeft" ! forAll {(rnge: NonEmptyList[List[Int]]) =>
    val F = Foldable1[NonEmptyList]
    rnge.list.reduceLeft(_++_) must_===(F.foldl1(rnge)(a => b => a ++ b))
  }

  "foldr1 is reduceRight" ! forAll {(rnge: NonEmptyList[List[Int]]) =>
    val F = Foldable1[NonEmptyList]
    rnge.list.reduceRight(_++_) must_===(F.foldr1(rnge)(a => b => a ++ b))
  }
  "foldRight1 is reduceRight" ! forAll { xs: NonEmptyList[List[Int]] =>
    val F = Foldable1[NonEmptyList]
    xs.list.reduceRight(_ ++ _) must_== F.foldRight1(xs)(_ ++ _)
  }
  "NonEmptyList.last is correct" ! forAll { xs:NonEmptyList[Int] =>
    xs.reverse.head must_===(xs.last)
  }
  "NonEmptyList.init size is correct" ! forAll { xs:NonEmptyList[Int] =>
    xs.init.size must_===(xs.tail.size)
  }
  "NonEmptyList.foldRight1 large list" in {
    import NonEmptyList._
    import syntax.foldable1._
    nel(0, List.fill(10000000)(1)).foldRight1(_ + _) must_== 10000000
  }
  "no stack overflow large list traverse" in {
    import syntax.traverse._
    val largeNel = NonEmptyList.nel(0, (1 to 100000).toList)
    (largeNel map Option.apply).sequence must_===(Option(largeNel))
  }
  "toNel is self" ! forAll { xs: NonEmptyList[Int] =>
    Foldable1[NonEmptyList].toNel(xs) must_=== xs
  }
}
