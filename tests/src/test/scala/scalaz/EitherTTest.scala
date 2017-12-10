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

  "rightU" should {
    val a: String \/ Int = \/-(1)
    val b: EitherT[({type l[a] = String \/ a})#l, Boolean, Int] = EitherT.rightU[Boolean](a)
    b must_== EitherT.rightT[({type l[a] = String \/ a})#l, Boolean, Int](a)
  }

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

  "fromDisjunction" ! forAll { (a: String \/ Int) =>
    Option(a.isLeft) must_=== EitherT.fromDisjunction[Option](a).isLeft
  }

  "either, pureLeft, pure" ! forAll { (a: String \/ Int) =>
    val e = EitherT.eitherT(Option(a))

    e must_=== {
      a match {
        case -\/(v) => EitherT.pureLeft(v)
        case \/-(v) => EitherT.pure(v)
      }
    }

    e must_=== EitherT.either(a)
  }

  "eitherT, leftT, rightT syntax" ! forAll { (a: String \/ Int) =>
    import scalaz.syntax.eithert._

    val e = EitherT.eitherT(Option(a))

    e must_=== {
      a match {
        case -\/(v) => v.leftT
        case \/-(v) => v.rightT
      }
    }

    e must_=== a.eitherT
  }

  "flatMapF consistent with flatMap" ! forAll { (a: EitherTList[Int, Int], f: Int => List[Int \/ String]) =>
    a.flatMap(f andThen EitherT.apply) must_=== a.flatMapF(f)
  }

  object instances {
    def functor[F[_] : Functor, A] = Functor[({type λ[α] = EitherT[F, A, α]})#λ]
    def monad[F[_] : Monad, A] = Monad[({type λ[α] = EitherT[F, A, α]})#λ]
    def plus[F[_] : Monad, A: Semigroup] = Plus[({type λ[α] = EitherT[F, A, α]})#λ]
    def monadPlus[F[_] : Monad, A: Monoid] = MonadPlus[({type λ[α] = EitherT[F, A, α]})#λ]
    def foldable[F[_] : Foldable, A] = Foldable[({type λ[α] = EitherT[F, A, α]})#λ]
    def traverse[F[_] : Traverse, A] = Traverse[({type λ[α] = EitherT[F, A, α]})#λ]
    def bifunctor[F[_] : Functor] = Bifunctor[({type λ[α, β] = EitherT[F, α, β]})#λ]
    def bifoldable[F[_] : Foldable] = Bifoldable[({type λ[α, β] = EitherT[F, α, β]})#λ]
    def bitraverse[F[_] : Traverse] = Bitraverse[({type λ[α, β] = EitherT[F, α, β]})#λ]

    // checking absence of ambiguity
    def functor[F[_] : Monad, A: Monoid] = Functor[({type λ[α] = EitherT[F, A, α]})#λ]
    def apply[F[_] : Monad, A: Monoid] = Apply[({type λ[α] = EitherT[F, A, α]})#λ]
    def monad[F[_] : Monad, A: Monoid] = Monad[({type λ[α] = EitherT[F, A, α]})#λ]
    def plus[F[_] : Monad, A: Monoid] = Plus[({type λ[α] = EitherT[F, A, α]})#λ]
    def foldable[F[_] : Traverse, A] = Foldable[({type λ[α] = EitherT[F, A, α]})#λ]
    def bifunctor[F[_] : Traverse] = Bifunctor[({type λ[α, β] = EitherT[F, α, β]})#λ]
    def bifoldable[F[_] : Traverse] = Bifoldable[({type λ[α, β] = EitherT[F, α, β]})#λ]
    def monadError[F[_] : Monad, A] = MonadError[({type λ[α, β] = EitherT[F, α, β] })#λ, A]
  }

  def compilationTests() = {
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

    //compilation test for eitherTU
    {
      val se: State[Vector[String], Int \/ Float] = null
      EitherT.eitherTU(se)
      val ee: String \/ (Int \/ Float) = null
      EitherT.eitherTU(ee)
    }
  }
}
