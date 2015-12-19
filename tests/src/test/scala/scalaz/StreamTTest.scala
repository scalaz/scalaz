package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object StreamTTest extends SpecLite {
  type StreamTOpt[A] = StreamT[Option, A]

  "fromStream / toStream" ! forAll {
    (ass: Stream[Stream[Int]]) =>
      StreamT.fromStream(ass).toStream must_===(ass)
  }

  "filter all" ! forAll {
    (ass: StreamT[Stream, Int]) =>
      ass.filter(_ => true) must_===(ass)
  }

  "isEmpty" ! forAll {
    (s: Stream[Int]) =>
      StreamT.fromStream(List(s)).isEmpty.forall(_ == s.isEmpty)
  }

  "filter none" ! forAll {
    (ass: StreamT[Stream, Int]) =>
      val filtered = ass.filter(_ => false)
      val isEmpty = filtered.isEmpty
      isEmpty.forall(_ == true)
  }
  
  "drop" ! forAll {
    (ass: Option[Stream[Int]], x: Int) =>
      StreamT.fromStream(ass).drop(x).toStream must_===(ass.map(_.drop(x)))
  }
  
  "take" ! forAll {
    (ass: Option[Stream[Int]], x: Int) =>
      StreamT.fromStream(ass).take(x).toStream must_===(ass.map(_.take(x)))
  }

  "mapM" ! forAll {
    (s: Stream[Int], l: List[Int]) => 
      val s0 = s map (_ + 1)
      StreamT.fromStream(List(s, s0)).mapM(i => l.map(_ + i)).toStream must_==(
        Traverse[Stream].traverse(s)(i => l.map(_ + i)) ::: 
        Traverse[Stream].traverse(s0)(i => l.map(_ + i))
      )
  }

  "foldMap" ! forAll {
    (s: Stream[Int]) =>
      import scalaz.Scalaz._
      StreamT.fromStream(s.some).foldMap(_.toString) must_==(s.foldMap(_.toString))
  }

  checkAll(equal.laws[StreamTOpt[Int]])
  checkAll(monoid.laws[StreamTOpt[Int]])
  checkAll(monadPlus.laws[StreamTOpt])
  checkAll(foldable.laws[StreamTOpt])
  
  object instances {
    def semigroup[F[_]: Functor, A] = Semigroup[StreamT[F, A]]
    def monoid[F[_]: Applicative, A] = Monoid[StreamT[F, A]]
    def functor[F[_]: Functor] = Functor[StreamT[F, ?]]
    def bind[F[_]: Functor] = Bind[StreamT[F, ?]]
    def plus[F[_]: Functor] = Plus[StreamT[F, ?]]
    def monad[F[_]: Applicative] = Monad[StreamT[F, ?]]
    def monadPlus[F[_]: Applicative] = MonadPlus[StreamT[F, ?]]
    def foldable[F[_]: Foldable] = Foldable[StreamT[F, ?]]

    // checking absence of ambiguity
    def semigroup[F[_]: Applicative, A] = Semigroup[StreamT[F, A]]
    def functor[F[_]: Applicative] = Functor[StreamT[F, ?]]
    def bind[F[_]: Applicative] = Bind[StreamT[F, ?]]
    def plus[F[_]: Applicative] = Plus[StreamT[F, ?]]
  }
}
