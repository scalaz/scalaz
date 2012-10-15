package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class ListTTest extends Spec {
  type ListTOpt[A] = ListT[Option, A]

  "fromList / toList" ! check {
    (ass: List[List[Int]]) =>
      ListT.fromList(ass).toList must be_===(ass)
  }

  "filter all" ! check {
    (ass: ListT[List, Int]) =>
      ass.filter(_ => true) must be_===(ass)
  }

  "filter none" ! check {
    (ass: ListT[List, Int]) =>
      val filtered = ass.filter(_ => false)
      val isEmpty = filtered.isEmpty
      isEmpty.toList.forall(identity)
  }
  
  "drop" ! check {
    (ass: Option[List[Int]], x: Int) =>
      ListT.fromList(ass).drop(x).toList must be_===(ass.map(_.drop(x)))
  }
  
  "take" ! check {
    (ass: Option[List[Int]], x: Int) =>
      ListT.fromList(ass).take(x).toList must be_===(ass.map(_.take(x)))
  }

  "map" ! check {
    (ass: List[List[Int]]) =>
      ListT.fromList(ass).map(_ * 2).toList must be_===(ass.map(_.map(_ * 2)))
  }

  "flatMap" ! check {
    (ass: List[List[Int]]) =>
      ListT.fromList(ass).flatMap(number => ListT.fromList(List(List(number.toFloat)))).toList must
      be_===(ass.map(_.flatMap(number => List(number.toFloat))))
  }

  // Exists to ensure that fromList and map don't stack overflow.
  "large map" ! {
    val list = (0 to 400).toList.map(_ => (0 to 400).toList)
    ListT.fromList(list).map(_ * 2).toList must be_===(list.map(_.map(_ * 2)))
  }

  checkAll(equal.laws[ListTOpt[Int]])
  checkAll(monoid.laws[ListTOpt[Int]])
  checkAll(plusEmpty.laws[ListTOpt])
  checkAll(monad.laws[ListTOpt])
  checkAll(monadPlus.laws[ListTOpt])

  object instances {
    def semigroup[F[+_]: Monad, A] = Semigroup[ListT[F, A]]
    def monoid[F[+_]: Monad, A] = Monoid[ListT[F, A]]
    def monad[F[+_]: Monad, A] = Monad[({type λ[α]=ListT[F, α]})#λ]
    def functor[F[+_]: Functor, A] = Functor[({type λ[α]=ListT[F, α]})#λ]

    // checking absence of ambiguity
    def functor[F[+_]: Monad, A] = Functor[({type λ[α]=ListT[F, α]})#λ]  
  }
}
