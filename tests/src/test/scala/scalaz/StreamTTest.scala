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

  checkAll(equal.laws[StreamTOpt[Int]])
  checkAll(monoid.laws[StreamTOpt[Int]])
  checkAll(monadPlus.laws[StreamTOpt])
  
  object instances {
    def semigroup[F[_]: Functor, A] = Semigroup[StreamT[F, A]]
    def monoid[F[_]: Applicative, A] = Monoid[StreamT[F, A]]
    def functor[F[_]: Functor, A] = Functor[({type λ[α]=StreamT[F, α]})#λ]
    def monad[F[_]: Applicative, A] = Monad[({type λ[α]=StreamT[F, α]})#λ]
    def monadPlus[F[_]: Applicative, A] = MonadPlus[({type λ[α]=StreamT[F, α]})#λ]
  }
}
