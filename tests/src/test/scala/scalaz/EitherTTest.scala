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

  object instances {
    def functor[F[_] : Functor, A] = Functor[({type λ[α] = EitherT[F, A, α]})#λ]
    def monad[F[_] : Monad, A] = Monad[({type λ[α] = EitherT[F, A, α]})#λ]
    def plus[F[_] : Monad, A: Semigroup] = Plus[({type λ[α] = EitherT[F, A, α]})#λ]
    def monadPlus[F[_] : Monad, A: Monoid] = MonadPlus[({type λ[α] = EitherT[F, A, α]})#λ]
    def foldable[F[_] : Foldable, A] = Foldable[({type λ[α] = EitherT[F, A, α]})#λ]
    def traverse[F[_] : Traverse, A] = Traverse[({type λ[α] = EitherT[F, A, α]})#λ]

    // checking absence of ambiguity
    def functor[F[_] : Monad, A: Monoid] = Functor[({type λ[α] = EitherT[F, A, α]})#λ]
    def apply[F[_] : Monad, A: Monoid] = Apply[({type λ[α] = EitherT[F, A, α]})#λ]
    def monad[F[_] : Monad, A: Monoid] = Monad[({type λ[α] = EitherT[F, A, α]})#λ]
    def plus[F[_] : Monad, A: Monoid] = Plus[({type λ[α] = EitherT[F, A, α]})#λ]
    def foldable[F[_] : Traverse, A] = Foldable[({type λ[α] = EitherT[F, A, α]})#λ]
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
