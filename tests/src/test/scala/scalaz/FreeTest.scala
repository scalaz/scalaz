package scalaz

import org.scalacheck.{Arbitrary, Gen}
import std.AllInstances._
import Free._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._

case class FreeList[A](f: Free[List, A])

sealed abstract class FreeListInstances {
  implicit def freeListTraverse = new Traverse[FreeList] {
    def traverseImpl[G[_], A, B](fa: FreeList[A])(f: A => G[B])(implicit G: Applicative[G]) =
      G.map(Traverse[Free[List, ?]].traverseImpl(fa.f)(f))(FreeList.apply)
  }
}

object FreeList extends FreeListInstances {

  implicit val freeListZip: Zip[FreeList] = new Zip[FreeList] {
    val Z = Zip[Free[List, ?]]
    override def zip[A, B](a: => FreeList[A], b: => FreeList[B]) =
      FreeList(Z.zip(a.f, b.f))
  }

  implicit def freeListMonad = new Monad[FreeList] with BindRec[FreeList] {
    def point[A](a: => A): FreeList[A] =
      FreeList(Monad[Free[List, ?]].point(a))

    def bind[A, B](fa: FreeList[A])(f: A => FreeList[B]): FreeList[B] =
      FreeList(Monad[Free[List, ?]].bind(fa.f) { a => f(a).f })

    def tailrecM[A, B](f: A => FreeList[A \/ B])(a: A): FreeList[B] =
      FreeList(BindRec[Free[List, ?]].tailrecM((x: A) => f(x).f)(a))
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

case class FreeOption[A](f: Free[Option, A])

object FreeOption {
  implicit def freeOptionBindRec: BindRec[FreeOption] = new BindRec[FreeOption] {
    def map[A, B](fa: FreeOption[A])(f: A => B): FreeOption[B] =
      FreeOption(Functor[Free[Option, ?]].map(fa.f)(f))

    def tailrecM[A, B](f: A => FreeOption[A \/ B])(a: A): FreeOption[B] =
      FreeOption(BindRec[Free[Option, ?]].tailrecM[A, B] { a => f(a).f }(a))

    def bind[A, B](fa: FreeOption[A])(f: A => FreeOption[B]): FreeOption[B] =
      FreeOption(Bind[Free[Option, ?]].bind(fa.f) { a => f(a).f })
  }

  implicit def freeOptionArb[A](implicit A: Arbitrary[A]): Arbitrary[FreeOption[A]] =
    Arbitrary(FreeTest.freeGen[Option, A](
      Gen.choose(0, 1).flatMap(Gen.listOfN(_, freeOptionArb[A].arbitrary.map(_.f)).map(_.headOption))
    ).map(FreeOption.apply))

  implicit def freeOptionEq[A](implicit A: Equal[A]): Equal[FreeOption[A]] = new Equal[FreeOption[A]] {
    def equal(a: FreeOption[A], b: FreeOption[A]) = Equal[Option[A]].equal(a.f.runRecM(identity), b.f.runRecM(identity))
  }
}

object FreeTest extends SpecLite {
  def freeGen[F[_], A](g: Gen[F[Free[F, A]]])(implicit A: Arbitrary[A]): Gen[Free[F, A]] =
    Gen.frequency(
      (1, Functor[Arbitrary].map(A)(Free.pure[F, A](_)).arbitrary),
      (1, Functor[Arbitrary].map(Arbitrary(g))(Free[F, A](_)).arbitrary)
    )

  "Option" should {
    checkAll(bindRec.laws[FreeOption])
  }

  "List" should {
    "not stack overflow with 50k binds" in {
      val expected = Applicative[FreeList].point(())
      val result =
        BindRec[FreeList].tailrecM((i: Int) =>
          if (i < 50000)
            Applicative[FreeList].point(\/.left[Int, Unit](i + 1))
          else
            Applicative[FreeList].point(\/.right[Int, Unit](()))
        )(0)

      Equal[FreeList[Unit]].equal(expected, result)
    }

    checkAll(traverse.laws[FreeList])
    checkAll(bindRec.laws[FreeList])
    checkAll(monad.laws[FreeList])
    checkAll(monoid.laws[FreeList[Int]])
    checkAll(semigroup.laws[FreeList[Int]])
    checkAll(zip.laws[FreeList])
  }

  object instances {
    def bindRec[F[_]] = BindRec[Free[F, ?]]
    def monad[F[_]] = Monad[Free[F, ?]]
    def foldable[F[_]: Foldable: Functor] = Foldable[Free[F, ?]]
    def foldable1[F[_]: Foldable1: Functor] = Foldable1[Free[F, ?]]
    def traverse[F[_]: Traverse] = Traverse[Free[F, ?]]
    def traverse1[F[_]: Traverse1] = Traverse1[Free[F, ?]]
    def monoid[F[_], A: Monoid] = Monoid[Free[F, A]]
    def semigroup[F[_], A: Semigroup] = Semigroup[Free[F, A]]

    // checking absence of ambiguity
    def functor[F[_]: Traverse1] = Functor[Free[F, ?]]
    def foldable[F[_]: Traverse1] = Foldable[Free[F, ?]]
    def foldable1[F[_]: Traverse1] = Foldable1[Free[F, ?]]
    def traverse[F[_]: Traverse1] = Traverse[Free[F, ?]]
    def semigroup[F[_], A: Monoid] = Semigroup[Free[F, A]]
  }
}
