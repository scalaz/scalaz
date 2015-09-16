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
    implicit def optionTArb0[F[_, _], E, A](implicit F: Arbitrary[F[E, Option[A]]]): Arbitrary[OptionT[({type l[a] = F[E, a]})#l, A]] =
      optionTArb[({type l[a] = F[E, a]})#l, A]

    implicit def optionTEqual0[F[_, _], E, A](implicit F: Equal[F[E, Option[A]]]): Equal[OptionT[({type l[a] = F[E, a]})#l, A]] =
      OptionT.optionTEqual[({type l[a] = F[E, a]})#l, A]

    checkAll(monadError.laws[({type x[E0, A] = OptionT[({type y[a] = E0 \/ a})#y, A]})#x, Int])
  }

  "show" ! forAll { a: OptionTList[Int] =>
    Show[OptionTList[Int]].show(a) must_=== Show[List[Option[Int]]].show(a.run)
  }
  
  "optionT" ! forAll { ass: List[Option[Int]] =>
      OptionT.optionT(ass).run == ass
  }

  "listT" ! forAll { a: OptionTList[Int] => a.toListT.run must_=== a.run.map(_.toList)}

  object instances {
    def functor[F[_] : Functor] = Functor[({type λ[α] = OptionT[F, α]})#λ]
    def monad[F[_] : Monad] = MonadPlus[({type λ[α] = OptionT[F, α]})#λ]
    def monadError[F[_, _], E](implicit F: MonadError[F, E]) = MonadError[({type x[E0, A] = OptionT[({type y[a] = F[E0, a]})#y, A]})#x, E]
    def foldable[F[_] : Foldable] = Foldable[({type λ[α] = OptionT[F, α]})#λ]
    def traverse[F[_] : Traverse] = Traverse[({type λ[α] = OptionT[F, α]})#λ]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[({type λ[α] = OptionT[F, α]})#λ]
    def functor[F[_] : Monad : Traverse] = Functor[({type λ[α] = OptionT[F, α]})#λ]
    def functor[F[_, _], E](implicit F1: MonadError[F, E], F2: Traverse[({type l[a] = F[E, a]})#l]) = Functor[({type x[a] = OptionT[({type y[b] = F[E, b]})#y, a]})#x]
    def apply[F[_] : Monad] = Apply[({type λ[α] = OptionT[F, α]})#λ]
    def foldable[F[_] : Traverse] = Foldable[({type λ[α] = OptionT[F, α]})#λ]
  }
}
