package scalaz

import org.scalacheck.Prop.{exists, propBoolean}

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import OnePlus.onePlusNelIso

class OnePlusTest extends Spec {
  type OnePlusList[A] = OnePlus[List, A]
  type OnePlusNel[A] = OnePlus[NonEmptyList, A]

  checkAll("OnePlus", equal.laws[OnePlus[List, Int]])
  checkAll("OnePlus", order.laws[OnePlus[List, Int]])
  checkAll("OnePlus List", traverse.laws[OnePlusList])
  checkAll("OnePlus Nel", traverse.laws[OnePlusNel])

  "onePlusNelIso is iso" ! prop {(nel: NonEmptyList[Int]) =>
    onePlusNelIso.from(onePlusNelIso.to(nel)) must be_===(nel)
  }

  "fold1 on fold0" ! prop {(ints: OnePlus[List, Int]) =>
    val lst = ints.head :: ints.tail
    Foldable[OnePlusList].foldMap(ints)(_ + 1) must be_===(lst.size + lst.sum)
    Foldable1[OnePlusList].foldMap1(ints)(_ + 1) must be_===(lst.size + lst.sum)
  }

  "fold1 on fold1" ! prop {(ints: OnePlus[NonEmptyList, Int]) =>
    val lst = ints.head :: ints.tail.list
    Foldable[OnePlusNel].foldMap(ints)(_ + 1) must be_===(lst.size + lst.sum)
    Foldable1[OnePlusNel].foldMap1(ints)(_ + 1) must be_===(lst.size + lst.sum)
  }

  "inequality exists" ! prop {(a: OnePlus[List, Int]) =>
    exists {(b: OnePlus[List, Int]) =>
      propBoolean(!Equal[OnePlusList[Int]].equal(a, b))
    }
  }

  object instances {
    def functor[F[_]: Functor, A] = Functor[({type λ[α] = OnePlus[F, α]})#λ]
    def foldable[F[_]: Foldable, A] = Foldable1[({type λ[α] = OnePlus[F, α]})#λ]
    def foldable1[F[_]: Foldable1, A] = Foldable1[({type λ[α] = OnePlus[F, α]})#λ]
    def traverse[F[_]: Traverse, A] = Traverse1[({type λ[α] = OnePlus[F, α]})#λ]
    def traverse1[F[_]: Traverse1, A] = Traverse1[({type λ[α] = OnePlus[F, α]})#λ]
    def each[F[_]: Each, A] = Each[({type λ[α] = OnePlus[F, α]})#λ]
  }
}
