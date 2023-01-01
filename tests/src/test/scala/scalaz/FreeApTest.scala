package scalaz

import org.scalacheck.{Arbitrary, Gen}
import scalaz.std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazArbitrary._

object FreeApTest extends SpecLite {
  private implicit def freeApArb[F[_], A](implicit A: Arbitrary[A], F: Arbitrary[F[A]]): Arbitrary[FreeAp[F, A]] =
    Arbitrary(
      Gen.oneOf(
        Functor[Gen].map(A.arbitrary)(FreeAp.pure[F, A]),
        Functor[Gen].map(F.arbitrary)(FreeAp.lift[F, A](_))
      )
    )

  private implicit def freeApEq[F[_]: Applicative, A](implicit F: Equal[F[A]]): Equal[FreeAp[F, A]] = {
    (a: FreeAp[F, A], b: FreeAp[F, A]) =>
      F.equal(a.retract, b.retract)
  }

  "Option" should {
    type FreeApOption[A] = FreeAp[Option, A]
    checkAll(applicative.laws[FreeApOption])
    checkAll(foldable.laws[FreeApOption])
  }

  "List" should {
    type FreeApList[A] = FreeAp[List, A]
    checkAll(applicative.laws[FreeApList])
    checkAll(foldable.laws[FreeApList])
  }

  "OneAnd[Maybe]" should {
    type FreeApOneAndMaybe[A] = FreeAp[OneAnd[Maybe, *], A]
    checkAll(applicative.laws[FreeApOneAndMaybe])
    checkAll(foldable1.laws[FreeApOneAndMaybe])
  }

  "NonEmptyList" should {
    type FreeApNel[A] = FreeAp[NonEmptyList, A]
    checkAll(applicative.laws[FreeApNel])
    checkAll(foldable1.laws[FreeApNel])
  }

  object instances {
    def applicative[F[_] ] = Applicative[FreeAp[F, *]]
    def foldable[F[_]: Foldable] = Foldable[FreeAp[F, *]]
    def foldable1[F[_]: Foldable1] = Foldable1[FreeAp[F, *]]

    def foldable[F[_]: Foldable1] = Foldable[FreeAp[F, *]]
  }
}
