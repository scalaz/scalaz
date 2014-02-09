package scalaz

import org.scalacheck.{Arbitrary, Gen}
import Isomorphism._
import std.AllInstances._
import Free.{Return, Suspend}
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazArbitrary._

object FreeTest extends SpecLite {

  implicit def freeArb[F[_], A](implicit A: Arbitrary[A], F: Arbitrary ~> ({type λ[α] = Arbitrary[F[α]]})#λ): Arbitrary[Free[F, A]] =
    Arbitrary(Gen.frequency(
      (1, Functor[Arbitrary].map(A)(Return[F, A](_)).arbitrary),
      (1, Functor[Arbitrary].map(F(freeArb[F, A]))(Suspend[F, A](_)).arbitrary)
    ))

  trait Template[F[_], G[_]] extends (G ~> ({type λ[α] = G[F[α]]})#λ) {
    override final def apply[A](a: G[A]) = lift(a)

    def lift[A: G]: G[F[A]]
  }

  implicit val listArb = new Template[List, Arbitrary] {
    def lift[A](implicit A: Arbitrary[A]) = Arbitrary(
      Gen.choose(0, 2).flatMap(Gen.listOfN(_, A.arbitrary)) // avoid stack overflow
    )
  }

  implicit val listEq = new Template[List, Equal] {
    def lift[A: Equal] = implicitly
  }

  implicit val oneAndOptArb = new Template[OneAndOpt, Arbitrary] {
    def lift[A: Arbitrary] = implicitly
  }

  implicit val oneAndOptEqual = new Template[OneAndOpt, Equal] {
    def lift[A: Equal] = implicitly
  }

  type OneAndOpt[A] = OneAnd[Option, A]

  "List" should {
    type FreeList[A] = Free[List, A]
    checkAll(traverse.laws[FreeList])
    checkAll(monad.laws[FreeList])
    checkAll(equal.laws[FreeList[Int]])
  }

  "OneAnd[Option, A]" should {
    type FreeOneAndOpt[A] = Free[OneAndOpt, A]

    checkAll(traverse1.laws[FreeOneAndOpt])
    checkAll(monad.laws[FreeOneAndOpt])
    checkAll(equal.laws[FreeOneAndOpt[Int]])
  }

  object instances {
    def monad[F[_]: Functor] = Monad[({type λ[α] = Free[F, α]})#λ]
    def foldable[F[_]: Foldable: Functor] = Foldable[({type λ[α] = Free[F, α]})#λ]
    def foldable1[F[_]: Foldable1: Functor] = Foldable1[({type λ[α] = Free[F, α]})#λ]
    def traverse[F[_]: Traverse] = Traverse[({type λ[α] = Free[F, α]})#λ]
    def traverse1[F[_]: Traverse1] = Traverse1[({type λ[α] = Free[F, α]})#λ]

    // checking absence of ambiguity
    def functor[F[_]: Traverse1] = Functor[({type λ[α] = Free[F, α]})#λ]
    def foldable[F[_]: Traverse1] = Foldable[({type λ[α] = Free[F, α]})#λ]
    def foldable1[F[_]: Traverse1] = Foldable1[({type λ[α] = Free[F, α]})#λ]
    def traverse[F[_]: Traverse1] = Traverse[({type λ[α] = Free[F, α]})#λ]
  }
}

