package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop.forAll

object OptionTTest extends SpecLite {

  type OptionTList[A] = OptionT[List, A]
  type OptionTOption[A] = OptionT[Option, A]

  checkAll(equal.laws[OptionTList[Int]])
  checkAll(monadPlus.laws[OptionTList])
  checkAll(traverse.laws[OptionTList])

  "show" ! forAll { a: OptionTList[Int] =>
    Show[OptionTList[Int]].show(a) must_=== Show[List[Option[Int]]].show(a.run)
  }
  
  "optionT" ! forAll { ass: List[Option[Int]] =>
      OptionT.optionT(ass).run == ass
  }

  object instances {
    def functor[F[_] : Functor] = Functor[OptionT[F, ?]]
    def monad[F[_] : Monad] = MonadPlus[OptionT[F, ?]]
    def foldable[F[_] : Foldable] = Foldable[OptionT[F, ?]]
    def traverse[F[_] : Traverse] = Traverse[OptionT[F, ?]]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[OptionT[F, ?]]
    def functor[F[_] : Monad : Traverse] = Functor[OptionT[F, ?]]
    def apply[F[_] : Monad] = Apply[OptionT[F, ?]]
    def foldable[F[_] : Traverse] = Foldable[OptionT[F, ?]]
  }
}
