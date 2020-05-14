package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import syntax.std.option._
import org.scalacheck.Prop.forAll

object OptionTTest extends SpecLite {

  type OptionTList[A] = OptionT[List, A]
  type OptionTOption[A] = OptionT[Option, A]
  type IntOr[A] = Int \/ A
  type OptionTEither[A] = OptionT[IntOr, A]
  type ConstInt[A] = Const[Int, A]

  checkAll(equal.laws[OptionTList[Int]])
  checkAll(bindRec.laws[OptionTList])
  checkAll(monadPlus.laws[OptionTList])
  checkAll(alt.laws[OptionTList])
  checkAll(traverse.laws[OptionTList])
  checkAll(monadError.laws[OptionTEither, Int])
  checkAll(decidable.laws[OptionT[ConstInt, *]])

  "show" ! forAll { (a: OptionTList[Int]) =>
    Show[OptionTList[Int]].show(a) must_=== Show[List[Option[Int]]].show(a.run)
  }

  "optionT" ! forAll { (ass: List[Option[Int]]) =>
      OptionT.optionT(ass).run == ass
  }

  "listT" ! forAll { (a: OptionTList[Int]) => a.toListT.run must_=== a.run.map(_.toIList)}

  "flatMapF consistent with flatMap" ! forAll { (fa: OptionTList[Int], f: Int => List[Option[Int]]) =>
    fa.flatMap(f andThen OptionT.apply) must_=== fa.flatMapF(f)
  }

  "mapF consistent with map" ! forAll { (fa: OptionTList[Int], f: Int => Int) =>
    fa.map(f) must_=== fa.mapF(f andThen (i => Applicative[List].point(i)))
  }

  object instances {
    def functor[F[_] : Functor] = Functor[OptionT[F, *]]
    def bindRec[F[_] : Monad: BindRec] = BindRec[OptionT[F, *]]
    def monad[F[_] : Monad] = MonadPlus[OptionT[F, *]]
    def alt[F[_] : Monad] = Alt[OptionT[F, *]]
    def monadError[F[_], E](implicit F: MonadError[F, E]) = MonadError[OptionT[F, *], E]
    def foldable[F[_] : Foldable] = Foldable[OptionT[F, *]]
    def traverse[F[_] : Traverse] = Traverse[OptionT[F, *]]
    def decidable[F[_] : Divisible] = Decidable[OptionT[F, *]]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[OptionT[F, *]]
    def functor[F[_] : Monad: BindRec] = Functor[OptionT[F, *]]
    def functor[F[_] : Monad : Traverse] = Functor[OptionT[F, *]]
    def functor[F[_], E](implicit F1: MonadError[F, E], F2: Traverse[F]) = Functor[OptionT[F, *]]
    def bind[F[_] : Monad: BindRec] = Bind[OptionT[F, *]]
    def apply[F[_] : Monad] = Apply[OptionT[F, *]]
    def foldable[F[_] : Traverse] = Foldable[OptionT[F, *]]
  }
}
