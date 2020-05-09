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
      G.map(Traverse[Free[List, *]].traverseImpl(fa.f)(f))(FreeList.apply)
  }
}

object FreeList extends FreeListInstances {

  implicit val freeListZip: Zip[FreeList] = new Zip[FreeList] {
    val Z = Zip[Free[List, *]]
    override def zip[A, B](a: => FreeList[A], b: => FreeList[B]) =
      FreeList(Z.zip(a.f, b.f))
  }

  implicit def freeListMonad = new Monad[FreeList] with BindRec[FreeList] {
    def point[A](a: => A): FreeList[A] =
      FreeList(Monad[Free[List, *]].point(a))

    def bind[A, B](fa: FreeList[A])(f: A => FreeList[B]): FreeList[B] =
      FreeList(Monad[Free[List, *]].bind(fa.f) { a => f(a).f })

    def tailrecM[A, B](a: A)(f: A => FreeList[A \/ B]): FreeList[B] =
      FreeList(BindRec[Free[List, *]].tailrecM(a)(f(_).f))
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
      FreeOption(Functor[Free[Option, *]].map(fa.f)(f))

    def tailrecM[A, B](a: A)(f: A => FreeOption[A \/ B]): FreeOption[B] =
      FreeOption(BindRec[Free[Option, *]].tailrecM(a) { f(_).f })

    def bind[A, B](fa: FreeOption[A])(f: A => FreeOption[B]): FreeOption[B] =
      FreeOption(Bind[Free[Option, *]].bind(fa.f) { a => f(a).f })
  }

  implicit def freeOptionArb[A](implicit A: Arbitrary[A]): Arbitrary[FreeOption[A]] =
    Arbitrary(FreeTest.freeGen[Option, A](
      Gen.choose(0, 1).flatMap(Gen.listOfN(_, freeOptionArb[A].arbitrary.map(_.f)).map(_.headOption))
    ).map(FreeOption.apply))

  implicit def freeOptionEq[A](implicit A: Equal[A]): Equal[FreeOption[A]] = new Equal[FreeOption[A]] {
    def equal(a: FreeOption[A], b: FreeOption[A]) = Equal[Option[A]].equal(a.f.runRecM(identity), b.f.runRecM(identity))
  }
}

object FreeState {
  /** stack-safe state monad */
  type FreeState[S, A] = Free[λ[α => S => (S, α)], A]

  def apply[S, A](f: S => (S, A)): FreeState[S, A] = Free.liftF[λ[α => S => (S, α)], A](f)

  implicit def monadState[S]: MonadState[FreeState[S, *], S] = new MonadState[FreeState[S, *], S] {
    type F[A] = S => (S, A)

    def point[A](a: => A): FreeState[S, A] = Free.liftF[F, A](s => (s, a))
    def bind[A, B](fa: FreeState[S, A])(f: A => FreeState[S, B]): FreeState[S, B] = fa.flatMap(f)

    def get: FreeState[S, S] = Free.liftF[F, S](s => (s, s))
    def put(s: S): FreeState[S, Unit] = Free.liftF[F, Unit](_ => (s, ()))
  }

  def run[S, A](fs: FreeState[S, A])(s: S): (S, A) = {
    type F[X] = (S, S => (S, X))
    fs.foldRun(s)(λ[F ~> (S, *)] { case (s, f) => f(s) })
  }
}

object FreeStateT {
  /** stack-safe state monad transformer */
  type FreeStateT[M[_], S, A] = Free[λ[α => S => M[(S, α)]], A]

  def apply[M[_], S, A](f: S => M[(S, A)]): FreeStateT[M, S, A] = Free.liftF[λ[α => S => M[(S, α)]], A](f)

  implicit def monadState[M[_], S](implicit M: Applicative[M]): MonadState[FreeStateT[M, S, *], S] =
    new MonadState[FreeStateT[M, S, *], S] {
      type F[A] = S => M[(S, A)]

      def point[A](a: => A): FreeStateT[M, S, A] = Free.liftF[F, A](s => M.point((s, a)))
      def bind[A, B](fa: FreeStateT[M, S, A])(f: A => FreeStateT[M, S, B]): FreeStateT[M, S, B] = fa.flatMap(f)

      def get: FreeStateT[M, S, S] = Free.liftF[F, S](s => M.point((s, s)))
      def put(s: S): FreeStateT[M, S, Unit] = Free.liftF[F, Unit](_ => M.point((s, ())))
    }

  def run[M[_]: Applicative: BindRec, S, A](fs: FreeStateT[M, S, A])(s: S): M[(S, A)] = {
    type F[X] = (S, S => M[(S, X)])
    type G[X] = M[(S, X)]
    fs.foldRunM(s)(λ[F ~> G] { case (s, f) => f(s) })
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
        BindRec[FreeList].tailrecM(0)(i =>
          if (i < 50000)
            Applicative[FreeList].point(\/.left[Int, Unit](i + 1))
          else
            Applicative[FreeList].point(\/.right[Int, Unit](()))
        )

      Equal[FreeList[Unit]].equal(expected, result)
    }

    checkAll(traverse.laws[FreeList])
    checkAll(bindRec.laws[FreeList])
    checkAll(monad.laws[FreeList])
    checkAll(monoid.laws[FreeList[Int]])
    checkAll(semigroup.laws[FreeList[Int]])
    checkAll(zip.laws[FreeList])
  }

  "FreeState" should {
    import FreeState._

    val ms: MonadState[FreeState[Int, *], Int] = FreeState.monadState

    "be stack-safe on left-associated binds" in {
      val go = (0 until 10000).foldLeft(ms.init)((fs, _) => fs.flatMap(_ => ms.state(i => (i+1, i+1))))

      10000 must_=== FreeState.run(go)(0)._1
    }

    "be stack-safe on right-associated (i.e. recursive) binds" in {
      def go: FreeState[Int, Int] =
        ms.state(n => (n-1, n-1)).flatMap(i =>
          if(i > 0) go
          else ms.state(n => (n, n))
        )

      0 must_=== FreeState.run(go)(10000)._2
    }
  }

  "FreeStateT" should {
    import FreeStateT._

    val ms: MonadState[FreeStateT[Option, Int, *], Int] = FreeStateT.monadState

    "be stack-safe on left-associated binds" in {
      val go = (0 until 10000).foldLeft(ms.init)((fs, _) => fs.flatMap(_ => ms.state(i => (i+1, i+1))))

      Option((10000, 10000)) must_=== FreeStateT.run(go)(0)
    }

    "be stack-safe on right-associated (i.e. recursive) binds" in {
      def go: FreeStateT[Option, Int, Int] =
        ms.state(n => (n-1, n-1)).flatMap(i =>
          if(i > 0) go
          else ms.state(n => (n, n))
        )

      Option((0, 0)) must_=== FreeStateT.run(go)(10000)
    }
  }

  "#1156: equals should not return true for obviously unequal instances" in {
    val a = Free.point[List, Int](1).flatMap(x => Free.point(2))
    val b = Free.point[List, Int](3).flatMap(x => Free.point(4))
    a != b
  }

  object instances {
    def bindRec[F[_]] = BindRec[Free[F, *]]
    def monad[F[_]] = Monad[Free[F, *]]
    def foldable[F[_]: Foldable: Functor] = Foldable[Free[F, *]]
    def foldable1[F[_]: Foldable1: Functor] = Foldable1[Free[F, *]]
    def traverse[F[_]: Traverse] = Traverse[Free[F, *]]
    def traverse1[F[_]: Traverse1] = Traverse1[Free[F, *]]
    def monoid[F[_], A: Monoid] = Monoid[Free[F, A]]
    def semigroup[F[_], A: Semigroup] = Semigroup[Free[F, A]]

    object trampoline {
      def comonad = Comonad[Free.Trampoline]
      def monad = Monad[Free.Trampoline]
    }
    object sink {
      def monad[S] = Monad[Free.Sink[S, *]]
    }
    object source {
      def monad[S] = Monad[Free.Source[S, *]]
    }

    // checking absence of ambiguity
    def functor[F[_]: Traverse1] = Functor[Free[F, *]]
    def foldable[F[_]: Traverse1] = Foldable[Free[F, *]]
    def foldable1[F[_]: Traverse1] = Foldable1[Free[F, *]]
    def traverse[F[_]: Traverse1] = Traverse[Free[F, *]]
    def semigroup[F[_], A: Monoid] = Semigroup[Free[F, A]]
  }
}
