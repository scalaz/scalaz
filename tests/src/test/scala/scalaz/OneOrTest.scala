package scalaz

import org.scalacheck.Prop.{exists, propBoolean}

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

class OneOrTest extends Spec {
  import OneOr._

  checkAll("OneOr", equal.laws[OneOr[List, Int]])
  checkAll("OneOr", order.laws[OneOr[List, Int]])
  checkAll("OneOr List", traverse.laws[OneOrList])
  checkAll("OneOr List", applicative.laws[OneOrList])
  checkAll("OneOr Nel", traverse.laws[OneOrNel])
  checkAll("OneOr Nel", comonad.laws[OneOrNel])

  "inequality exists" ! prop {(a: OneOrList[Int]) =>
    exists {(b: OneOrList[Int]) =>
      propBoolean(!Equal[OneOrList[Int]].equal(a, b))
    }
  }

  object instances {
    def functor[F[_]: Functor, A] = Functor[({type λ[α] = OneOr[F, α]})#λ]
    def apply[F[_]: Apply, A] = Apply[({type λ[α] = OneOr[F, α]})#λ]
    def applicative[F[_]: Apply, A] = Applicative[({type λ[α] = OneOr[F, α]})#λ]
    def cobind[F[_]: Cobind, A] = Cobind[({type λ[α] = OneOr[F, α]})#λ]
    def comonad[F[_]: Comonad, A] = Comonad[({type λ[α] = OneOr[F, α]})#λ]
    def foldable[F[_]: Foldable, A] = Foldable[({type λ[α] = OneOr[F, α]})#λ]
    def foldable1[F[_]: Foldable1, A] = Foldable1[({type λ[α] = OneOr[F, α]})#λ]
    def traverse[F[_]: Traverse, A] = Traverse[({type λ[α] = OneOr[F, α]})#λ]
    def traverse1[F[_]: Traverse1, A] = Traverse1[({type λ[α] = OneOr[F, α]})#λ]
  }

}
