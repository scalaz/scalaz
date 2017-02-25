package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop.forAll

object NonEmptyListTest extends SpecLite {
  checkAll("NonEmptyList", monad.laws[NonEmptyList])
  checkAll("NonEmptyList", bindRec.laws[NonEmptyList])
  checkAll("NonEmptyList", plus.laws[NonEmptyList])
  checkAll("NonEmptyList", semigroup.laws[NonEmptyList[Int]])
  checkAll("NonEmptyList", equal.laws[NonEmptyList[Int]])
  checkAll("NonEmptyList", order.laws[NonEmptyList[Int]])
  checkAll("NonEmptyList", traverse1.laws[NonEmptyList])
  checkAll("NonEmptyList", FoldableTests.anyAndAllLazy[NonEmptyList])
  checkAll("NonEmptyList", zip.laws[NonEmptyList])
  checkAll("NonEmptyList", align.laws[NonEmptyList])
  checkAll("NonEmptyList", comonad.laws[NonEmptyList])

  "scanLeft1" ! forAll { fa: NonEmptyList[List[Int]] =>
    def f[A]: (List[A], List[A]) => List[A] = _ ::: _
    val a = Foldable1[NonEmptyList].scanLeft1(fa)(f)
    a.list must_=== fa.tail.scanLeft(fa.head)(f)
    a.size must_=== fa.size
  }

  "scanRight1" ! forAll { fa: NonEmptyList[List[Int]] =>
    def f[A]: (List[A], List[A]) => List[A] = _ ::: _
    val a = Foldable1[NonEmptyList].scanRight1(fa)(f)
    a.list must_=== fa.init.scanRight(fa.last)(f)
    a.size must_=== fa.size
  }

  "findLeft/findRight" in {
    val a = NonEmptyList(1, 2, 3, 4, 5)
    Foldable[NonEmptyList].findLeft(a)(_ % 2 == 0) must_=== Some(2)
    Foldable[NonEmptyList].findRight(a)(_ % 2 == 0) must_=== Some(4)
  }

  "findLeft" ! forAll{ a: NonEmptyList[Int] =>
    val f = (_: Int) % 3 == 0
    Foldable[NonEmptyList].findLeft(a)(f) must_=== Foldable[IList].findLeft(a.list)(f)
  }

  "findRight" ! forAll { a: NonEmptyList[Int] =>
    val f = (_: Int) % 3 == 0
    Foldable[NonEmptyList].findRight(a)(f) must_=== Foldable[IList].findRight(a.list)(f)
  }

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
  "correctness of tails" ! forAll { xs: NonEmptyList[Int] =>
    import NonEmptyList._
    xs.tails must_=== nel(xs, xs.tail match {
      case INil() => INil()
      case ICons(h, t) => nel(h, t).tails.list
    })
  }
  "toNel is self" ! forAll { xs: NonEmptyList[Int] =>
    Foldable1[NonEmptyList].toNel(xs) must_=== xs
  }
  "zipWithIndex" ! forAll { xs: NonEmptyList[Int] =>
    xs.zipWithIndex.list must_== xs.list.zipWithIndex
  }
}
