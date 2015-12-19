package scalaz

import Leibniz.===

object UnapplyTest extends SpecLite {
  object unapply {
    val ue = Unapply[Monad, Int \/ String]
    def mequiv[A] = implicitly[ue.M[A] === (Int \/ A)]
    implicitly[ue.A === String]

    // needs only transient stable type
    Unapply[Monad, Int \/ String].TC : Monad[Int \/ ?]
  }

  object unapply2 {
    val ue = Unapply2[Arrow, Kleisli[NonEmptyList, Int, String]]
    def mequiv[A,B] = implicitly[ue.M[A,B] === Kleisli[NonEmptyList, A, B]]
    implicitly[ue.A === Int]
    implicitly[ue.B === String]

    // needs only transient stable type
    Unapply2[Arrow, Kleisli[NonEmptyList, Int, String]].TC: Arrow[Kleisli[NonEmptyList, ?, ?]]
  }

  object unapplyProduct {
    val ue = UnapplyProduct[Applicative, Writer[IList[String], Int], Writer[IList[String], Char]]
    def mequiv[A] = implicitly[ue.M[A] === Writer[IList[String], A]]
    implicitly[ue.A === Int]
    implicitly[ue.B === Char]

    // needs only transient stable type
    UnapplyProduct[Applicative, Writer[IList[String], Int], Writer[IList[String], Char]].TC: Applicative[Writer[IList[String], ?]]
  }
}
