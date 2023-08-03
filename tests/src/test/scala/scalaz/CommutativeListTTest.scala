package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object CommutativeListTTest extends SpecLite {
  type CommutativeListTOpt[A] = CommutativeListT[Option, A]
  type ConstInt[A] = Const[Int, A]

  "fromList / toList" ! forAll {
    (ass: List[List[Int]]) =>
      CommutativeListT.fromList(ass).toList must_===(ass)
  }

  "fromIList / toIList" ! forAll {
    (ass: IList[IList[Int]]) =>
    CommutativeListT.fromIList(ass).toIList must_===(ass)
  }

  "filter all" ! forAll {
    (ass: CommutativeListT[List, Int]) =>
      ass.filter(_ => true) must_===(ass)
  }

  "filter none" ! forAll {
    (ass: CommutativeListT[List, Int]) =>
      val filtered = ass.filter(_ => false)
      val isEmpty = filtered.isEmpty
      isEmpty.toList.forall(identity)
  }

  "find" ! forAll {
    (ass: CommutativeListTOpt[Int]) =>
      ass.find(_ > 0 ) must_=== MaybeT.maybeT(ass.run.map(_.find( _ > 0)))
  }

  "drop" ! forAll {
    (ass: Option[List[Int]], x: Int) =>
      CommutativeListT.fromList(ass).drop(x).toList must_===(ass.map(_.drop(x)))
  }

  "take" ! forAll {
    (ass: Option[List[Int]], x: Int) =>
      CommutativeListT.fromList(ass).take(x).toList must_===(ass.map(_.take(x)))
  }

  "map" ! forAll {
    (ass: List[List[Int]]) =>
      CommutativeListT.fromList(ass).map(_ * 2).toList must_===(ass.map(_.map(_ * 2)))
  }

  "mapF consistent with map" ! forAll { (fa: CommutativeListTOpt[Int], f: Int => Int) =>
    fa.map(f) must_=== fa.mapF(f andThen (i => Applicative[Option].point(i)))
  }

  "collect" ! forAll {
    (ass: List[List[Int]]) =>
      val pf : PartialFunction[Int, String] = { case (i : Int) if i > 2 => i.toString }
      CommutativeListT.fromList(ass).collect(pf).toList must_===(ass.map(_.collect(pf)))
  }

  "flatMap" ! forAll {
    (ass: List[List[Int]]) =>
      (CommutativeListT.fromList(ass).flatMap(number => CommutativeListT.fromList(List(List(number.toFloat)))).toList
      must_===(ass.map(_.flatMap(number => List(number.toFloat)))))
  }

  "flatMapF consistent with flatMap" ! forAll { (fa: CommutativeListTOpt[Int], f: Int => Option[IList[String]]) =>
    fa.flatMap(f andThen CommutativeListT.apply) must_=== fa.flatMapF(f)
  }

  // Exists to ensure that fromList and map don't stack overflow.
  "large map" ! {
    val list = (0 to 400).toList.map(_ => (0 to 400).toList)
    CommutativeListT.fromList(list).map(_ * 2).toList must_===(list.map(_.map(_ * 2)))
    ()
  }

  "commutativeListT" ! forAll {
    (ass: Option[IList[Int]]) =>
      CommutativeListT.commutativeListT(ass).run == ass
  }

  checkAll(equal.laws[CommutativeListTOpt[Int]])
  checkAll(monoid.laws[CommutativeListTOpt[Int]])
  checkAll(plusEmpty.laws[CommutativeListTOpt])
  checkAll(monad.laws[CommutativeListTOpt])
  checkAll(monadPlus.laws[CommutativeListTOpt])
  checkAll(alt.laws[CommutativeListTOpt])
  checkAll(monadTrans.laws[CommutativeListT, Option])
  checkAll(decidable.laws[CommutativeListT[ConstInt, *]])

  object instances {
    def semigroup[F[_]: Monad, A] = Semigroup[CommutativeListT[F, A]]
    def monoid[F[_]: Monad, A] = Monoid[CommutativeListT[F, A]]
    def monad[F[_]: Monad] = Monad[CommutativeListT[F, *]]
    def functor[F[_]: Functor] = Functor[CommutativeListT[F, *]]
    def alt[F[_]: Monad] = Alt[CommutativeListT[F, *]]
    def decidable[F[_] : Divisible] = Decidable[CommutativeListT[F, *]]

    // checking absence of ambiguity
    def functor[F[_]: Monad] = Functor[CommutativeListT[F, *]]
  }
}
