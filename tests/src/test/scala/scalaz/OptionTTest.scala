package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

object OptionTTest extends SpecLite {

  type OptionTList[A] = OptionT[List, A]
  type OptionTOption[A] = OptionT[Option, A]

  checkAll(equal.laws[OptionTList[Int]])
  checkAll(monadPlus.laws[OptionTList])
  checkAll(traverse.laws[OptionTList])

  {
    implicit def optionTArb0[F[_, _], E, A](implicit F: Arbitrary[F[E, Option[A]]]): Arbitrary[OptionT[F[E, ?], A]] =
      optionTArb[F[E, ?], A]

    implicit def optionTEqual0[F[_, _], E, A](implicit F: Equal[F[E, Option[A]]]): Equal[OptionT[F[E, ?], A]] =
      OptionT.optionTEqual[F[E, ?], A]

    checkAll(monadError.laws[λ[(E0, A) => OptionT[E0 \/ ?, A]], Int])
  }

  "show" ! forAll { a: OptionTList[Int] =>
    Show[OptionTList[Int]].show(a) must_=== Show[List[Option[Int]]].show(a.run)
  }
  
  "optionT" ! forAll { ass: List[Option[Int]] =>
      OptionT.optionT(ass).run == ass
  }

  object instances {
    def functor[F[_] : Functor] = Functor[OptionT[F, ?]]
    def monad[F[_] : Monad] = MonadPlus[OptionT[F, ?]]
    def monadError[F[_, _], E](implicit F: MonadError[F, E]) = MonadError[λ[(E0, A) => OptionT[F[E0, ?], A]], E]
    def foldable[F[_] : Foldable] = Foldable[OptionT[F, ?]]
    def traverse[F[_] : Traverse] = Traverse[OptionT[F, ?]]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[OptionT[F, ?]]
    def functor[F[_] : Monad : Traverse] = Functor[OptionT[F, ?]]
    def functor[F[_, _], E](implicit F1: MonadError[F, E], F2: Traverse[F[E, ?]]) = Functor[OptionT[λ[A => F[E, A]], ?]]
    def apply[F[_] : Monad] = Apply[OptionT[F, ?]]
    def foldable[F[_] : Traverse] = Foldable[OptionT[F, ?]]
  }
}
