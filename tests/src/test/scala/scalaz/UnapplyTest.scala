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

  object unapply3 {
    implicit def functorInstance[A]: Functor[λ[α => A]] = ???
    implicitly[Unapply[Functor, String]]
  }

  object unapply4 {
    class M0[F[_], A0, B0, C0, D0, E0]
    implicit def functorInstance: Functor[M0[Option, Int, Int, Int, Int, ?]] = ???
    implicitly[Unapply[Functor, M0[Option, Int, Int, Int, Int, String]]]
  }

  object unapply5 {
    class M0[F[_], A0, A1, B0]
    implicit def functorInstance: Functor[λ[α => M0[Option, α, α, String]]] = ???
    implicitly[Unapply[Functor, M0[Option, Int, Int, String]]]
  }

  object unapply6 {
    class M0[F[_], A0, B0, C0]
    implicit def functorInstance: Functor[M0[Option, Int, Int, ?]] = ???
    implicitly[Unapply[Functor, M0[Option, Int, Int, String]]]
  }

  object unapply7 {
    class M0[F[_], A0, B0]
    implicit def functorInstance: Functor[M0[Option, ?, String]] = ???
    implicitly[Unapply[Functor, M0[Option, Int, String]]]
  }

  object unapply8 {
    class M0[F[_], A0, B0]
    implicit def functorInstance: Functor[M0[Option, Int, ?]] = ???
    implicitly[Unapply[Functor, M0[Option, Int, String]]]
  }

  object unapply9 {
    class M0[F[_], A0]
    implicit def functorInstance: Functor[M0[Option, ?]] = ???
    implicitly[Unapply[Functor, M0[Option, String]]]
  }

  object unapply10 {
    class M0[A0, B0, C0, D0, E0, F0, G0]
    implicit def functorInstance: Functor[M0[Int, Int, Int, Int, Int, Int, ?]] = ???
    implicitly[Unapply[Functor, M0[Int, Int, Int, Int, Int, Int, String]]]
  }

  object unapply11 {
    class M0[A0, B0, C0, D0, E0, F0]
    implicit def functorInstance: Functor[M0[Int, Int, Int, Int, Int, ?]] = ???
    implicitly[Unapply[Functor, M0[Int, Int, Int, Int, Int, String]]]
  }

  object unapply12 {
    class M0[A0, B0, C0, D0, E0]
    implicit def functorInstance: Functor[M0[Int, Int, Int, Int, ?]] = ???
    implicitly[Unapply[Functor, M0[Int, Int, Int, Int, String]]]
  }

  object unapply13 {
    class M0[A0, B0, C0, D0]
    implicit def functorInstance: Functor[M0[Int, Int, Int, ?]] = ???
    implicitly[Unapply[Functor, M0[Int, Int, Int, String]]]
  }

  object unapply14 {
    class M0[A0, B0, C0]
    implicit def functorInstance: Functor[M0[Int, Int, ?]] = ???
    implicitly[Unapply[Functor, M0[Int, Int, String]]]
  }

  object unapply15 {
    class M0[A0, B0]
    implicit def functorInstance: Functor[M0[Int, ?]] = ???
    implicitly[Unapply[Functor, M0[Int, String]]]
  }

  object unapply16 {
    class M0[A0, B0]
    implicit def functorInstance: Functor[M0[?, String]] = ???
    implicitly[Unapply[Functor, M0[Int, String]]]
  }

  object unapply17 {
    class MT[F[_], A]
    class MAB[A, B]
    implicit def functorInstance: Functor[MT[MAB[Int, ?], ?]] = ???
    implicitly[Unapply[Functor, MT[MAB[Int, ?], String]]]
  }

  object unapply2_0 {
    class M0[F[_], A0, B0]
    implicit def bifunctorInstance: Bifunctor[M0[Option, ?, ?]] = ???
    implicitly[Unapply2[Bifunctor, M0[Option, Int, String]]]
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
