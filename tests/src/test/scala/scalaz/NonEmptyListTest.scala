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

  "distinct" ! forAll { xs: NonEmptyList[Int] =>
    Option(xs.distinct) must_=== std.list.toNel(Foldable[NonEmptyList].toList(xs).distinct)
  }

  "NonEmptyList size is correct" ! forAll { xs:NonEmptyList[Int] =>
    xs.size must_===(1 + xs.tail.count(b => true)) 
  }

  "foldl1 is reduceLeft" ! forAll {(rnge: NonEmptyList[IList[Int]]) =>
    val F = Foldable1[NonEmptyList]
    rnge.list.toList.reduceLeft(_++_) must_===(F.foldl1(rnge)(a => b => a ++ b))
  }

  "foldr1 is reduceRight" ! forAll {(rnge: NonEmptyList[IList[Int]]) =>
    val F = Foldable1[NonEmptyList]
    rnge.list.toList.reduceRight(_++_) must_===(F.foldr1(rnge)(a => b => a ++ b))
  }
  "foldRight1 is reduceRight" ! forAll { xs: NonEmptyList[IList[Int]] =>
    val F = Foldable1[NonEmptyList]
    xs.list.toList.reduceRight(_ ++ _) must_== F.foldRight1(xs)(_ ++ _)
  }
  "NonEmptyList.last is correct" ! forAll { xs:NonEmptyList[Int] =>
    xs.reverse.head must_===(xs.last)
  }
  "NonEmptyList.init size is correct" ! forAll { xs:NonEmptyList[Int] =>
    xs.init.count(a => true) must_===(xs.tail.count(a => true))
  }
  "NonEmptyList.foldRight1 large list" in {
    import NonEmptyList._
    import syntax.foldable1._
    nel(0, IList.fromList(List.fill(10000000)(1))).foldRight1(_ + _) must_== 10000000
  }
  "no stack overflow large list traverse" in {
    import syntax.traverse._
    val largeNel = NonEmptyList.nel(0, IList.fromList((1 to 100000).toList))
    (largeNel map Option.apply).sequence must_===(Option(largeNel))
  }
  "toNel is self" ! forAll { xs: NonEmptyList[Int] =>
    Foldable1[NonEmptyList].toNel(xs) must_=== xs
  }
}
