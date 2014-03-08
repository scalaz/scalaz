package scalaz

import std.option._, std.anyVal._
import org.scalacheck.Arbitrary
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._

object CoyonedaTest extends SpecLite {

  implicit def coyonedaArb[F[_], A](implicit A: Arbitrary[F[A]]): Arbitrary[Coyoneda[F, A]] =
    Functor[Arbitrary].map(A)(Coyoneda.apply)

  type CoyonedaOption[A] = Coyoneda[Option, A]
  type CoyonedaNel[A] = Coyoneda[NonEmptyList, A]

  checkAll(monadPlus.strongLaws[CoyonedaOption])
  checkAll(cobind.laws[CoyonedaOption])
  checkAll(traverse.laws[CoyonedaOption])
  checkAll(order.laws[Coyoneda[Option, Int]])
  checkAll(foldable.laws[CoyonedaOption](implicitly, Coyoneda.coyonedaFoldable, implicitly))

  checkAll(monad.laws[CoyonedaNel])
  checkAll(plus.laws[CoyonedaNel])
  checkAll(comonad.laws[CoyonedaNel])
  checkAll(traverse1.laws[CoyonedaNel])
  checkAll(order.laws[Coyoneda[NonEmptyList, Int]])
  checkAll(foldable1.laws[CoyonedaNel](implicitly, Coyoneda.coyonedaFoldable1, implicitly))

  object instances {
    def functor[F[_]] = Functor[({type λ[α] = Coyoneda[F, α]})#λ]
    def contravariant[F[_]: Functor: Contravariant] = Contravariant[({type λ[α] = Coyoneda[F, α]})#λ]
    def foldable[F[_]: Foldable] = Foldable[({type λ[α] = Coyoneda[F, α]})#λ]
    def foldable1[F[_]: Foldable1] = Foldable1[({type λ[α] = Coyoneda[F, α]})#λ]
    def traverse[F[_]: Traverse] = Traverse[({type λ[α] = Coyoneda[F, α]})#λ]
    def traverse1[F[_]: Traverse1] = Traverse1[({type λ[α] = Coyoneda[F, α]})#λ]
    def apply[F[_]: Apply] = Apply[({type λ[α] = Coyoneda[F, α]})#λ]
    def applicative[F[_]: Applicative] = Applicative[({type λ[α] = Coyoneda[F, α]})#λ]
    def applicativePlus[F[_]: ApplicativePlus] = ApplicativePlus[({type λ[α] = Coyoneda[F, α]})#λ]
    def bind[F[_]: Bind] = Bind[({type λ[α] = Coyoneda[F, α]})#λ]
    def monad[F[_]: Monad] = Monad[({type λ[α] = Coyoneda[F, α]})#λ]
    def monadPlus[F[_]: MonadPlus] = MonadPlus[({type λ[α] = Coyoneda[F, α]})#λ]
    def plus[F[_]: Functor: Plus] = Plus[({type λ[α] = Coyoneda[F, α]})#λ]
    def plusEmpty[F[_]: Functor: PlusEmpty] = PlusEmpty[({type λ[α] = Coyoneda[F, α]})#λ]
    def cobind[F[_]: Cobind] = Cobind[({type λ[α] = Coyoneda[F, α]})#λ]
    def comonad[F[_]: Comonad] = Comonad[({type λ[α] = Coyoneda[F, α]})#λ]
    def equal[F[_], A](implicit F: Functor[F], E: Equal[F[A]]) = Equal[Coyoneda[F, A]]
    def order[F[_], A](implicit F: Functor[F], E: Order[F[A]]) = Order[Coyoneda[F, A]]

    // checking absence of ambiguity
    def functor[F[_]: MonadPlus: Comonad: Traverse1] = Functor[({type λ[α] = Coyoneda[F, α]})#λ]
    def foldable[F[_]: Foldable1] = Foldable[({type λ[α] = Coyoneda[F, α]})#λ]
    def foldable[F[_]: Traverse1] = Foldable[({type λ[α] = Coyoneda[F, α]})#λ]
    def foldable1[F[_]: Traverse1] = Foldable1[({type λ[α] = Coyoneda[F, α]})#λ]
    def traverse[F[_]: Traverse1] = Traverse[({type λ[α] = Coyoneda[F, α]})#λ]
    def apply[F[_]: MonadPlus] = Apply[({type λ[α] = Coyoneda[F, α]})#λ]
    def applicative[F[_]: MonadPlus] = Applicative[({type λ[α] = Coyoneda[F, α]})#λ]
    def applicativePlus[F[_]: MonadPlus] = ApplicativePlus[({type λ[α] = Coyoneda[F, α]})#λ]
    def bind[F[_]: MonadPlus] = Bind[({type λ[α] = Coyoneda[F, α]})#λ]
    def monad[F[_]: MonadPlus] = Monad[({type λ[α] = Coyoneda[F, α]})#λ]
    def plus[F[_]: MonadPlus] = Plus[({type λ[α] = Coyoneda[F, α]})#λ]
    def plusEmpty[F[_]: MonadPlus] = PlusEmpty[({type λ[α] = Coyoneda[F, α]})#λ]
    def cobind[F[_]: Comonad] = Cobind[({type λ[α] = Coyoneda[F, α]})#λ]
    def equal[F[_], A](implicit F: Functor[F], E: Order[F[A]]) = Equal[Coyoneda[F, A]]
  }

}

