package scalaz

import org.scalacheck.Prop._

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop.forAll

object OneOrTest extends SpecLite {
  import OneOr._
  checkAll("OneOr", equal.laws[OneOr[List, Int]])
  checkAll("OneOr", order.laws[OneOr[List, Int]])
  checkAll("OneOr List", traverse.laws[OneOrList])
  checkAll("OneOr List", applicative.laws[OneOrList])
  checkAll("OneOr Nel", traverse1.laws[OneOrNel])
  checkAll("OneOr Nel", comonad.laws[OneOrNel])

//  "inequality exists" ! forAll {(a: OneOrList[Int]) =>
//    exists {(b: OneOrList[Int]) =>
//      propBoolean(!Equal[OneOrList[Int]].equal(a, b))
//    }
//  }

  object instances {
    def functor[F[_]: Functor] = Functor[OneOr[F, ?]]
    def apply[F[_]: Apply] = Apply[OneOr[F, ?]]
    def applicative[F[_]: Apply] = Applicative[OneOr[F, ?]]
    def cobind[F[_]: Cobind] = Cobind[OneOr[F, ?]]
    def comonad[F[_]: Comonad] = Comonad[OneOr[F, ?]]
    def foldable[F[_]: Foldable] = Foldable[OneOr[F, ?]]
    def foldable1[F[_]: Foldable1] = Foldable1[OneOr[F, ?]]
    def traverse[F[_]: Traverse] = Traverse[OneOr[F, ?]]
    def traverse1[F[_]: Traverse1] = Traverse1[OneOr[F, ?]]
  }

}
