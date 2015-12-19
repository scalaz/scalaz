package scalaz

import std.option._, std.anyVal._
import org.scalacheck.Arbitrary
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._

object CoyonedaTest extends SpecLite {

  implicit def coyonedaArb[F[_], A](implicit A: Arbitrary[F[A]]): Arbitrary[Coyoneda[F, A]] =
    Functor[Arbitrary].map(A)(Coyoneda.lift)

  type CoyonedaOption[A] = Coyoneda[Option, A]
  type CoyonedaNel[A] = Coyoneda[NonEmptyList, A]

  checkAll(monadPlus.strongLaws[CoyonedaOption])
  checkAll(bindRec.laws[CoyonedaOption])
  checkAll(cobind.laws[CoyonedaOption])
  checkAll(traverse.laws[CoyonedaOption])
  checkAll(order.laws[Coyoneda[Option, Int]])
  checkAll(foldable.laws[CoyonedaOption](implicitly, Coyoneda.coyonedaFoldable, implicitly))

  checkAll(monad.laws[CoyonedaNel])
  checkAll(bindRec.laws[CoyonedaNel])
  checkAll(plus.laws[CoyonedaNel])
  checkAll(comonad.laws[CoyonedaNel])
  checkAll(traverse1.laws[CoyonedaNel])
  checkAll(order.laws[Coyoneda[NonEmptyList, Int]])
  checkAll(foldable1.laws[CoyonedaNel](implicitly, Coyoneda.coyonedaFoldable1, implicitly))

  object instances {
    def functor[F[_]] = Functor[Coyoneda[F, ?]]
    def contravariant[F[_]: Functor: Contravariant] = Contravariant[Coyoneda[F, ?]]
    def foldable[F[_]: Foldable] = Foldable[Coyoneda[F, ?]]
    def foldable1[F[_]: Foldable1] = Foldable1[Coyoneda[F, ?]]
    def traverse[F[_]: Traverse] = Traverse[Coyoneda[F, ?]]
    def traverse1[F[_]: Traverse1] = Traverse1[Coyoneda[F, ?]]
    def apply[F[_]: Apply] = Apply[Coyoneda[F, ?]]
    def applicative[F[_]: Applicative] = Applicative[Coyoneda[F, ?]]
    def applicativePlus[F[_]: ApplicativePlus] = ApplicativePlus[Coyoneda[F, ?]]
    def bind[F[_]: Bind] = Bind[Coyoneda[F, ?]]
    def bindRec[F[_]: BindRec] = BindRec[Coyoneda[F, ?]]
    def monad[F[_]: Monad] = Monad[Coyoneda[F, ?]]
    def monadPlus[F[_]: MonadPlus] = MonadPlus[Coyoneda[F, ?]]
    def plus[F[_]: Functor: Plus] = Plus[Coyoneda[F, ?]]
    def plusEmpty[F[_]: Functor: PlusEmpty] = PlusEmpty[Coyoneda[F, ?]]
    def cobind[F[_]: Cobind] = Cobind[Coyoneda[F, ?]]
    def comonad[F[_]: Comonad] = Comonad[Coyoneda[F, ?]]
    def equal[F[_], A](implicit F: Functor[F], E: Equal[F[A]]) = Equal[Coyoneda[F, A]]
    def order[F[_], A](implicit F: Functor[F], E: Order[F[A]]) = Order[Coyoneda[F, A]]

    // checking absence of ambiguity
    def functor[F[_]: MonadPlus: Comonad: Traverse1] = Functor[Coyoneda[F, ?]]
    def foldable[F[_]: Foldable1] = Foldable[Coyoneda[F, ?]]
    def foldable[F[_]: Traverse1] = Foldable[Coyoneda[F, ?]]
    def foldable1[F[_]: Traverse1] = Foldable1[Coyoneda[F, ?]]
    def traverse[F[_]: Traverse1] = Traverse[Coyoneda[F, ?]]
    def apply[F[_]: MonadPlus] = Apply[Coyoneda[F, ?]]
    def applicative[F[_]: MonadPlus] = Applicative[Coyoneda[F, ?]]
    def applicativePlus[F[_]: MonadPlus] = ApplicativePlus[Coyoneda[F, ?]]
    def bind[F[_]: MonadPlus] = Bind[Coyoneda[F, ?]]
    def monad[F[_]: MonadPlus] = Monad[Coyoneda[F, ?]]
    def plus[F[_]: MonadPlus] = Plus[Coyoneda[F, ?]]
    def plusEmpty[F[_]: MonadPlus] = PlusEmpty[Coyoneda[F, ?]]
    def cobind[F[_]: Comonad] = Cobind[Coyoneda[F, ?]]
    def equal[F[_], A](implicit F: Functor[F], E: Order[F[A]]) = Equal[Coyoneda[F, A]]
  }

}

