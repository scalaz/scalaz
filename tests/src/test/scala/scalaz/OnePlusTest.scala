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
  checkAll("OnePlus List", monad.laws[OnePlusList])
  checkAll("OnePlus List", plus.laws[OnePlusList])
  checkAll("OnePlus Nel", plus.laws[OnePlusNel])
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

  "right fold1 on fold" ! prop {(ints: OnePlus[List, Int]) =>
    val lst = ints.head :: ints.tail
    val llst = Functor[OnePlusList].map(ints)(List(_))
    Foldable[OnePlusList].foldRight(ints, List.empty[Int])(_ :: _) must be_===(lst)
    Foldable1[OnePlusList].foldRight1(llst)(_ ++ _) must be_===(lst)
  }

  "right fold1 on fold1" ! prop {(ints: OnePlus[NonEmptyList, Int]) =>
    val lst = ints.head :: ints.tail.list
    val llst = Functor[OnePlusNel].map(ints)(List(_))
    Foldable[OnePlusNel].foldRight(ints, List.empty[Int])(_ :: _) must be_===(lst)
    Foldable1[OnePlusNel].foldRight1(llst)(_ ++ _) must be_===(lst)
  }

  "traverse1 on traverse" ! prop {(ints: OnePlus[List, Int], f: Int => List[Int]) =>
    (Traverse1[OnePlusList].traverse1(ints)(f)
       must_==(Traverse[OnePlusList].traverse(ints)(f)))
  }

  "traverse1 on traverse1" ! prop {(ints: OnePlus[NonEmptyList, Int],
                                    f: Int => NonEmptyList[Int]) =>
    (Traverse1[OnePlusNel].traverse1(ints)(f)
       must_==(Traverse[OnePlusNel].traverse(ints)(f)))
  }

  "inequality exists" ! prop {(a: OnePlus[List, Int]) =>
    exists {(b: OnePlus[List, Int]) =>
      propBoolean(!Equal[OnePlusList[Int]].equal(a, b))
    }
  }

  object instances {
    def functor[F[_]: Functor, A] = Functor[({type λ[α] = OnePlus[F, α]})#λ]
    def apply[F[_]: Apply, A] = Apply[({type λ[α] = OnePlus[F, α]})#λ]
    def applicative[F[_]: ApplicativePlus, A] = Applicative[({type λ[α] = OnePlus[F, α]})#λ]
    def bind[F[_]: Monad: Plus, A] = Bind[({type λ[α] = OnePlus[F, α]})#λ]
    def monad[F[_]: MonadPlus, A] = Monad[({type λ[α] = OnePlus[F, α]})#λ]
    def plus[F[_]: Applicative: Plus, A] = Plus[({type λ[α] = OnePlus[F, α]})#λ]
    def foldable[F[_]: Foldable, A] = Foldable1[({type λ[α] = OnePlus[F, α]})#λ]
    def foldable1[F[_]: Foldable1, A] = Foldable1[({type λ[α] = OnePlus[F, α]})#λ]
    def traverse[F[_]: Traverse, A] = Traverse1[({type λ[α] = OnePlus[F, α]})#λ]
    def traverse1[F[_]: Traverse1, A] = Traverse1[({type λ[α] = OnePlus[F, α]})#λ]
    def each[F[_]: Each, A] = Each[({type λ[α] = OnePlus[F, α]})#λ]
  }
}
