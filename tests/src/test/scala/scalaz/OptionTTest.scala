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

  object instances {
    def functor[F[_] : Functor] = Functor[({type λ[α] = OptionT[F, α]})#λ]
    def monad[F[_] : Monad] = MonadPlus[({type λ[α] = OptionT[F, α]})#λ]
    def foldable[F[_] : Foldable] = Foldable[({type λ[α] = OptionT[F, α]})#λ]
    def traverse[F[_] : Traverse] = Traverse[({type λ[α] = OptionT[F, α]})#λ]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[({type λ[α] = OptionT[F, α]})#λ]
    def functor[F[_] : Monad : Traverse] = Functor[({type λ[α] = OptionT[F, α]})#λ]
    def apply[F[_] : Monad] = Apply[({type λ[α] = OptionT[F, α]})#λ]
    def foldable[F[_] : Traverse] = Foldable[({type λ[α] = OptionT[F, α]})#λ]
  }
}
