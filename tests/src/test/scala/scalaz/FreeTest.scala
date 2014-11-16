package scalaz

import org.scalacheck.{Arbitrary, Gen}
import std.AllInstances._
import Free.{Return, Suspend}
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._

case class FreeList[A](f: Free[List, A])

object FreeList {
  implicit def freeListTraverse = new Traverse[FreeList] {
    def traverseImpl[G[_], A, B](fa: FreeList[A])(f: A => G[B])(implicit G: Applicative[G]) =
      G.map(Traverse[({type λ[α] = Free[List, α]})#λ].traverseImpl(fa.f)(f))(FreeList.apply)
  }

  implicit def freeListMonad = new Monad[FreeList] {
    def point[A](a: => A): FreeList[A] = FreeList(Monad[({type λ[α] =
      Free[List, α]})#λ].point(a))

    def bind[A, B](fa: FreeList[A])(f: A => FreeList[B]): FreeList[B] =
      FreeList(Monad[({type λ[α] = Free[List, α]})#λ].bind(fa.f) { a => f(a).f })
  }

  implicit def freeListArb[A](implicit A: Arbitrary[A]): Arbitrary[FreeList[A]] =
    Arbitrary(FreeTest.freeGen[List, A](
      Gen.choose(0, 2).flatMap(Gen.listOfN(_, freeListArb[A].arbitrary.map(_.f)))
    ).map(FreeList.apply))

  implicit def freeListEq[A](implicit A: Equal[A]): Equal[FreeList[A]] = new Equal[FreeList[A]] {
    def equal(a: FreeList[A], b: FreeList[A]) = Equal[List[A]].equal(a.f.runM(identity), b.f.runM(identity))
  }

  implicit def freeListMonoid[A:Monoid]: Monoid[FreeList[A]] = new Monoid[FreeList[A]] {
    def zero = FreeList(Monoid[Free[List, A]].zero)

    def append(f1: FreeList[A], f2: => FreeList[A]) =
      FreeList(Monoid[Free[List, A]].append(f1.f, f2.f))
  }

  implicit def freeListSemigroup[A:Semigroup]: Semigroup[FreeList[A]] = new Semigroup[FreeList[A]] {
    def append(f1: FreeList[A], f2: => FreeList[A]) =
      FreeList(Semigroup[Free[List, A]].append(f1.f, f2.f))
  }
}

object FreeTest extends SpecLite {
  implicit def freeGen[F[_], A](g: Gen[F[Free[F, A]]])(implicit A: Arbitrary[A]): Gen[Free[F, A]] =
    Gen.frequency(
      (1, Functor[Arbitrary].map(A)(Return[F, A](_)).arbitrary),
      (1, Functor[Arbitrary].map(Arbitrary(g))(Suspend[F, A](_)).arbitrary)
    )

  "List" should {
    checkAll(traverse.laws[FreeList])
    checkAll(monad.laws[FreeList])
    checkAll(monoid.laws[FreeList[Int]])
    checkAll(semigroup.laws[FreeList[Int]])
  }

  object instances {
    def monad[F[_]: Functor] = Monad[({type λ[α] = Free[F, α]})#λ]
    def foldable[F[_]: Foldable: Functor] = Foldable[({type λ[α] = Free[F, α]})#λ]
    def foldable1[F[_]: Foldable1: Functor] = Foldable1[({type λ[α] = Free[F, α]})#λ]
    def traverse[F[_]: Traverse] = Traverse[({type λ[α] = Free[F, α]})#λ]
    def traverse1[F[_]: Traverse1] = Traverse1[({type λ[α] = Free[F, α]})#λ]
    def monoid[F[_]: Functor, A: Monoid] = Monoid[Free[F, A]]
    def semigroup[F[_]: Functor, A: Semigroup] = Semigroup[Free[F, A]]

    // checking absence of ambiguity
    def functor[F[_]: Traverse1] = Functor[({type λ[α] = Free[F, α]})#λ]
    def foldable[F[_]: Traverse1] = Foldable[({type λ[α] = Free[F, α]})#λ]
    def foldable1[F[_]: Traverse1] = Foldable1[({type λ[α] = Free[F, α]})#λ]
    def traverse[F[_]: Traverse1] = Traverse[({type λ[α] = Free[F, α]})#λ]
    def semigroup[F[_]: Functor, A: Monoid] = Semigroup[Free[F, A]]
  }
}
