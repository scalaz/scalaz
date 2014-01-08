package scalaz

import Leibniz.===

import org.scalacheck.{Prop, Gen}
import org.scalacheck.Prop.forAll

object UnapplyTest extends SpecLite {
  object unapply {
    val ue = Unapply[Monad, Int \/ String]
    def mequiv[A] = implicitly[ue.M[A] === (Int \/ A)]
      implicitly[ue.A === String]

    // needs only transient stable type
    Unapply[Monad, Int \/ String].TC : Monad[({type λ[α] = Int \/ α})#λ]
  }

  object unapply2 {
    import std.function._
    val ue = Unapply2[Arrow, Kleisli[NonEmptyList, Int, String]]
    def mequiv[A,B] = implicitly[ue.M[A,B] === Kleisli[NonEmptyList, A, B]]
    implicitly[ue.A === Int]
    implicitly[ue.B === String]

    // needs only transient stable type
    Unapply2[Arrow, Kleisli[NonEmptyList, Int, String]].TC
        : Arrow[({type λ[α,β] = Kleisli[NonEmptyList, α, β]})#λ]
  }
}
