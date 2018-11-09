package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object ListTTest extends SpecLite {
  type ListTOpt[A] = ListT[Option, A]
  type ConstInt[A] = Const[Int, A]

  "fromList / toList" ! forAll {
    (ass: List[List[Int]]) =>
      ListT.fromList(ass).toList must_===(ass)
  }

  "fromIList / toIList" ! forAll {
    (ass: IList[IList[Int]]) =>
    ListT.fromIList(ass).toIList must_===(ass)
  }

  "filter all" ! forAll {
    (ass: ListT[List, Int]) =>
      ass.filter(_ => true) must_===(ass)
  }

  "filter none" ! forAll {
    (ass: ListT[List, Int]) =>
      val filtered = ass.filter(_ => false)
      val isEmpty = filtered.isEmpty
      isEmpty.toList.forall(identity)
  }

  "find" ! forAll {
    (ass: ListTOpt[Int]) =>
      ass.find(_ > 0 ) must_===(OptionT.optionT(ass.run.map(_.find( _ > 0))))
  }

  "drop" ! forAll {
    (ass: Option[List[Int]], x: Int) =>
      ListT.fromList(ass).drop(x).toList must_===(ass.map(_.drop(x)))
  }

  "take" ! forAll {
    (ass: Option[List[Int]], x: Int) =>
      ListT.fromList(ass).take(x).toList must_===(ass.map(_.take(x)))
  }

  "map" ! forAll {
    (ass: List[List[Int]]) =>
      ListT.fromList(ass).map(_ * 2).toList must_===(ass.map(_.map(_ * 2)))
  }

  "mapF consistent with map" ! forAll { (fa: ListTOpt[Int], f: Int => Int) =>
    fa.map(f) must_=== fa.mapF(f andThen (i => Applicative[Option].point(i)))
  }

  "collect" ! forAll {
    (ass: List[List[Int]]) =>
      val pf : PartialFunction[Int, String] = { case (i : Int) if i > 2 => i.toString }
      ListT.fromList(ass).collect(pf).toList must_===(ass.map(_.collect(pf)))
  }

  "flatMap" ! forAll {
    (ass: List[List[Int]]) =>
      (ListT.fromList(ass).flatMap(number => ListT.fromList(List(List(number.toFloat)))).toList
      must_===(ass.map(_.flatMap(number => List(number.toFloat)))))
  }

  "flatMapF consistent with flatMap" ! forAll { (fa: ListTOpt[Int], f: Int => Option[IList[String]]) =>
    fa.flatMap(f andThen ListT.apply) must_=== fa.flatMapF(f)
  }

  // Exists to ensure that fromList and map don't stack overflow.
  "large map" ! {
    val list = (0 to 400).toList.map(_ => (0 to 400).toList)
    ListT.fromList(list).map(_ * 2).toList must_===(list.map(_.map(_ * 2)))
    ()
  }

  "listT" ! forAll {
    (ass: Option[IList[Int]]) =>
      ListT.listT(ass).run == ass
  }

  checkAll(equal.laws[ListTOpt[Int]])
  checkAll(monoid.laws[ListTOpt[Int]])
  checkAll(plusEmpty.laws[ListTOpt])
  checkAll(monad.laws[ListTOpt])
  checkAll(monadPlus.laws[ListTOpt])
  checkAll(alt.laws[ListTOpt])
  checkAll(monadTrans.laws[ListT, Option])
  checkAll(decidable.laws[ListT[ConstInt, ?]])

  object instances {
    def semigroup[F[_]: Monad, A] = Semigroup[ListT[F, A]]
    def monoid[F[_]: Monad, A] = Monoid[ListT[F, A]]
    def monad[F[_]: Monad] = Monad[ListT[F, ?]]
    def functor[F[_]: Functor] = Functor[ListT[F, ?]]
    def alt[F[_]: Monad] = Alt[ListT[F, ?]]
    def decidable[F[_] : Divisible] = Decidable[ListT[F, ?]]

    // checking absence of ambiguity
    def functor[F[_]: Monad] = Functor[ListT[F, ?]]
  }
}
