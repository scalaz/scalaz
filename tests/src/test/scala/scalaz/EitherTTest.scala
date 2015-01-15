package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop.forAll

object EitherTTest extends SpecLite {

  type EitherTList[A, B] = EitherT[List, A, B]
  type EitherTListInt[A] = EitherT[List, Int, A]
  type EitherTOptionInt[A] = EitherT[Option, Int, A]

  checkAll(equal.laws[EitherTListInt[Int]])
  checkAll(monadPlus.laws[EitherTListInt])
  checkAll(traverse.laws[EitherTListInt])
  checkAll(bitraverse.laws[EitherTList])
  checkAll(monadError.laws[EitherTList, Int])

  "consistent Bifoldable" ! forAll { a: EitherTList[Int, Int] =>
    val F = new Bitraverse[EitherTList]{
      def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: EitherTList[A, B])(f: A => G[C], g: B => G[D]) =
        EitherT.eitherTBitraverse[List].bitraverseImpl(fab)(f, g)
    }

    Bifoldable[EitherTList].bifoldMap(a)(_ :: Nil)(_ :: Nil) must_=== F.bifoldMap(a)(_ :: Nil)(_ :: Nil)
  }

  "show" ! forAll { a: EitherTList[Int, Int] =>
    Show[EitherTList[Int, Int]].show(a) must_=== Show[List[Int \/ Int]].show(a.run)
  }

  "flatMapF consistent with flatMap" ! forAll { (a: EitherTList[Int, Int], f: Int => List[Int \/ String]) =>
    a.flatMap(f andThen EitherT.apply) must_=== a.flatMapF(f)
  }

  object instances {
    def functor[F[_] : Functor, A] = Functor[EitherT[F, A, ?]]
    def monad[F[_] : Monad, A] = Monad[EitherT[F, A, ?]]
    def plus[F[_] : Monad, A: Semigroup] = Plus[EitherT[F, A, ?]]
    def monadPlus[F[_] : Monad, A: Monoid] = MonadPlus[EitherT[F, A, ?]]
    def foldable[F[_] : Foldable, A] = Foldable[EitherT[F, A, ?]]
    def traverse[F[_] : Traverse, A] = Traverse[EitherT[F, A, ?]]
    def bifunctor[F[_] : Functor] = Bifunctor[EitherT[F, ?, ?]]
    def bifoldable[F[_] : Foldable] = Bifoldable[EitherT[F, ?, ?]]
    def bitraverse[F[_] : Traverse] = Bitraverse[EitherT[F, ?, ?]]

    // checking absence of ambiguity
    def functor[F[_] : Monad, A: Monoid] = Functor[EitherT[F, A, ?]]
    def apply[F[_] : Monad, A: Monoid] = Apply[EitherT[F, A, ?]]
    def monad[F[_] : Monad, A: Monoid] = Monad[EitherT[F, A, ?]]
    def plus[F[_] : Monad, A: Monoid] = Plus[EitherT[F, A, ?]]
    def foldable[F[_] : Traverse, A] = Foldable[EitherT[F, A, ?]]
    def bifunctor[F[_] : Traverse] = Bifunctor[EitherT[F, ?, ?]]
    def bifoldable[F[_] : Traverse] = Bifoldable[EitherT[F, ?, ?]]
    def monadError[F[_] : Monad, A] = MonadError[EitherT[F, ?, ?], A]
  }

  // compilation test
  // https://gist.github.com/vmarquez/5106252/
  {
    import scalaz.syntax.either._

    case class ABC(s:String)

    implicit val m = new Monoid[(ABC, Int)] {
      def zero: (ABC, Int) = (null, -1)
      def append(f1: (ABC, Int), f2: => (ABC, Int)): (ABC, Int) = f1
    }

    def brokenMethod: EitherT[Option, (ABC, Int), (ABC, String)] =
      EitherT(Some((ABC("abcData"),"Success").right))

    def filterComp =
      brokenMethod
      .filter {
        case (abc,"Success") => true
        case _ => false
      }.map {
        case (abc, "Success") => "yay"
      }

    for {
      (a,b) <- brokenMethod
    } yield "yay"
  }
}
