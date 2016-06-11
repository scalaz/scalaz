package scalaz

import org.scalacheck.Prop.{exists, propBoolean}

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import OneAnd.oneAndNelIso
import org.scalacheck.Prop.forAll

object OneAndTest extends SpecLite {
  type OneAndOption[A] = OneAnd[Option, A]
  type OneAndList[A] = OneAnd[List, A]
  type OneAndNel[A] = OneAnd[NonEmptyList, A]

  checkAll("OneAnd", equal.laws[OneAnd[List, Int]])
  checkAll("OneAnd", order.laws[OneAnd[List, Int]])
  checkAll("OneAnd List", monad.laws[OneAndList])
  checkAll("OneAnd Option", monad.laws[OneAndOption])
  checkAll("OneAnd Nel", plus.laws[OneAndNel])
  checkAll("OneAnd List", traverse1.laws[OneAndList])
  checkAll("OneAnd Nel", traverse1.laws[OneAndNel])
  checkAll("OneAnd List", zip.laws[OneAndList])
  checkAll("OneAnd Nel", zip.laws[OneAndNel])
  checkAll("OneAnd List", align.laws[OneAndList])
  checkAll("OneAnd Nel", align.laws[OneAndNel])
  checkAll("OneAnd List", semigroup.laws[OneAnd[List, Int]])
  checkAll("OneAnd Nel", semigroup.laws[OneAnd[NonEmptyList, Int]])
  checkAll("OneAnd Option", semigroup.laws[OneAnd[Option, Int]])

  checkAll(FoldableTests.anyAndAllLazy[OneAndList])

  "findLeft/findRight" in {
    val a = OneAnd(1, List(2, 3, 4, 5))
    Foldable[OneAnd[List, ?]].findLeft(a)(_ % 2 == 0) must_=== Some(2)
    Foldable[OneAnd[List, ?]].findRight(a)(_ % 2 == 0) must_=== Some(4)
  }

  "findLeft" ! forAll{ a: OneAnd[List, Int] =>
    val f = (_: Int) % 3 == 0
    val F = Foldable[OneAnd[List, ?]]
    F.findLeft(a)(f) must_=== Foldable[IList].findLeft(F.toIList(a))(f)
  }

  "findRight" ! forAll { a: OneAnd[List, Int] =>
    val f = (_: Int) % 3 == 0
    val F = Foldable[OneAnd[List, ?]]
    F.findRight(a)(f) must_=== Foldable[IList].findRight(F.toIList(a))(f)
  }

  "oneAndNelIso is iso" ! forAll {(nel: NonEmptyList[Int]) =>
    oneAndNelIso.from(oneAndNelIso.to(nel)) must_===(nel)
  }

  "fold1 on fold0" ! forAll {(ints: OneAnd[List, Int]) =>
    val lst = ints.head :: ints.tail
    Foldable[OneAndList].foldMap(ints)(_ + 1) must_===(lst.size + lst.sum)
    Foldable1[OneAndList].foldMap1(ints)(_ + 1) must_===(lst.size + lst.sum)
  }

  "fold1 on fold1" ! forAll {(ints: OneAnd[NonEmptyList, Int]) =>
    val lst = ints.head :: ints.tail.list
    Foldable[OneAndNel].foldMap(ints)(_ + 1) must_===(lst.count(_ => true) + lst.toList.sum)
    Foldable1[OneAndNel].foldMap1(ints)(_ + 1) must_===(lst.count(_ => true) + lst.toList.sum)
  }

  "right fold1 on fold" ! forAll {(ints: OneAnd[List, Int]) =>
    val lst = ints.head :: ints.tail
    val llst = Functor[OneAndList].map(ints)(List(_))
    Foldable[OneAndList].foldRight(ints, List.empty[Int])(_ :: _) must_===(lst)
    Foldable1[OneAndList].foldRight1(llst)(_ ++ _) must_===(lst)
  }

  "right fold1 on fold1" ! forAll {(ints: OneAnd[NonEmptyList, Int]) =>
    val lst = ints.head :: ints.tail.list.toList
    val llst = Functor[OneAndNel].map(ints)(List(_))
    Foldable[OneAndNel].foldRight(ints, List.empty[Int])(_ :: _) must_===(lst)
    Foldable1[OneAndNel].foldRight1(llst)(_ ++ _) must_===(lst)
  }

  "traverse1 on traverse" ! forAll {(ints: OneAnd[List, Int], f: Int => List[Int]) =>
    (Traverse1[OneAndList].traverse1(ints)(f)
       must_==(Traverse[OneAndList].traverse(ints)(f)))
  }

  implicit def OneAndNelEqual[A](implicit E: Equal[IList[A]]): Equal[NonEmptyList[OneAndNel[A]]] = new Equal[NonEmptyList[OneAndNel[A]]] {
    override def equal(a: NonEmptyList[OneAndNel[A]], b: NonEmptyList[OneAndNel[A]]): Boolean =
      Equal[IList[A]].equal(a.map(oa => oa.head +: oa.tail.list).list.flatten, b.map(bb => bb.head +: bb.tail.list).list.flatten)
  }

  "traverse1 on traverse1" ! forAll {(ints: OneAnd[NonEmptyList, Int],
                                    f: Int => NonEmptyList[Int]) =>
    (Traverse1[OneAndNel].traverse1(ints)(f)
       must_===(Traverse[OneAndNel].traverse(ints)(f)))
  }

  "inequality exists" ! forAll {(a: OneAnd[List, Int]) =>
    exists {(b: OneAnd[List, Int]) =>
      propBoolean(!Equal[OneAndList[Int]].equal(a, b))
    }
  }

  object instances {
    def functor[F[_]: Functor] = Functor[OneAnd[F, ?]]
    def functorMax[F[_]: MonadPlus: Traverse1] = Functor[OneAnd[F, ?]]
    def apply[F[_]: Applicative: Plus] = Apply[OneAnd[F, ?]]
    def applicative[F[_]: ApplicativePlus] = Applicative[OneAnd[F, ?]]
    def bind[F[_]: Monad: Plus] = Bind[OneAnd[F, ?]]
    def monad[F[_]: MonadPlus] = Monad[OneAnd[F, ?]]
    def plus[F[_]: Applicative: Plus] = Plus[OneAnd[F, ?]]
    def foldable[F[_]: Foldable] = Foldable1[OneAnd[F, ?]]
    def foldable1[F[_]: Foldable1] = Foldable1[OneAnd[F, ?]]
    def traverse[F[_]: Traverse] = Traverse1[OneAnd[F, ?]]
    def traverse1[F[_]: Traverse1] = Traverse1[OneAnd[F, ?]]
    def semigroup[F[_]: Applicative: Plus, A] = Semigroup[OneAnd[F, A]]
  }
}
