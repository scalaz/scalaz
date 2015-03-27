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
    def functor[F[_]: Functor, A] = Functor[OneOr[F, ?]]
    def apply[F[_]: Apply, A] = Apply[OneOr[F, ?]]
    def applicative[F[_]: Apply, A] = Applicative[OneOr[F, ?]]
    def cobind[F[_]: Cobind, A] = Cobind[OneOr[F, ?]]
    def comonad[F[_]: Comonad, A] = Comonad[OneOr[F, ?]]
    def foldable[F[_]: Foldable, A] = Foldable[OneOr[F, ?]]
    def foldable1[F[_]: Foldable1, A] = Foldable1[OneOr[F, ?]]
    def traverse[F[_]: Traverse, A] = Traverse[OneOr[F, ?]]
    def traverse1[F[_]: Traverse1, A] = Traverse1[OneOr[F, ?]]
  }

}
