package scalaz

import org.scalacheck.Prop.{exists, propBoolean}

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import OneAnd.oneAndNelIso

class OneAndTest extends Spec {
  type OneAndList[A] = OneAnd[List, A]
  type OneAndNel[A] = OneAnd[NonEmptyList, A]

  checkAll("OneAnd", equal.laws[OneAnd[List, Int]])
  checkAll("OneAnd", order.laws[OneAnd[List, Int]])
  checkAll("OneAnd List", traverse.laws[OneAndList])
  checkAll("OneAnd Nel", traverse.laws[OneAndNel])

  "oneAndNelIso is iso" ! prop {(nel: NonEmptyList[Int]) =>
    oneAndNelIso.from(oneAndNelIso.to(nel)) must be_===(nel)
  }

  "fold1 on fold0" ! prop {(ints: OneAnd[List, Int]) =>
    val lst = ints.head :: ints.tail
    Foldable[OneAndList].foldMap(ints)(_ + 1) must be_===(lst.size + lst.sum)
    Foldable1[OneAndList].foldMap1(ints)(_ + 1) must be_===(lst.size + lst.sum)
  }

  "fold1 on fold1" ! prop {(ints: OneAnd[NonEmptyList, Int]) =>
    val lst = ints.head :: ints.tail.list
    Foldable[OneAndNel].foldMap(ints)(_ + 1) must be_===(lst.size + lst.sum)
    Foldable1[OneAndNel].foldMap1(ints)(_ + 1) must be_===(lst.size + lst.sum)
  }

  "right fold1 on fold" ! prop {(ints: OneAnd[List, Int]) =>
    val lst = ints.head :: ints.tail
    val llst = Functor[OneAndList].map(ints)(List(_))
    Foldable[OneAndList].foldRight(ints, List.empty[Int])(_ :: _) must be_===(lst)
    Foldable1[OneAndList].foldRight1(llst)(_ ++ _) must be_===(lst)
  }

  "right fold1 on fold1" ! prop {(ints: OneAnd[NonEmptyList, Int]) =>
    val lst = ints.head :: ints.tail.list
    val llst = Functor[OneAndNel].map(ints)(List(_))
    Foldable[OneAndNel].foldRight(ints, List.empty[Int])(_ :: _) must be_===(lst)
    Foldable1[OneAndNel].foldRight1(llst)(_ ++ _) must be_===(lst)
  }

  "traverse1 on traverse" ! prop {(ints: OneAnd[List, Int], f: Int => List[Int]) =>
    (Traverse1[OneAndList].traverse1(ints)(f)
       must_==(Traverse[OneAndList].traverse(ints)(f)))
  }

  "traverse1 on traverse1" ! prop {(ints: OneAnd[NonEmptyList, Int],
                                    f: Int => NonEmptyList[Int]) =>
    (Traverse1[OneAndNel].traverse1(ints)(f)
       must_==(Traverse[OneAndNel].traverse(ints)(f)))
  }

  "inequality exists" ! prop {(a: OneAnd[List, Int]) =>
    exists {(b: OneAnd[List, Int]) =>
      propBoolean(!Equal[OneAndList[Int]].equal(a, b))
    }
  }

  object instances {
    def functor[F[_]: Functor, A] = Functor[({type λ[α] = OneAnd[F, α]})#λ]
    def foldable[F[_]: Foldable, A] = Foldable1[({type λ[α] = OneAnd[F, α]})#λ]
    def foldable1[F[_]: Foldable1, A] = Foldable1[({type λ[α] = OneAnd[F, α]})#λ]
    def traverse[F[_]: Traverse, A] = Traverse1[({type λ[α] = OneAnd[F, α]})#λ]
    def traverse1[F[_]: Traverse1, A] = Traverse1[({type λ[α] = OneAnd[F, α]})#λ]
    def each[F[_]: Each, A] = Each[({type λ[α] = OneAnd[F, α]})#λ]
  }
}
