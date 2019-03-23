package scalaz


object UnapplyTest extends SpecLite {
  def teq[A[_], B[X] >: A[X] <: A[X]]: Unit = ()
  def teq2[A[_, _], B[X, Y] >: A[X, Y] <: A[X, Y]]: Unit = ()

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
    val u = Unapply[Functor, String]
    teq[u.M, λ[α => String]]
  }

  object unapply4 {
    class M0[F[_], A0, B0, C0, D0, E0]
    implicit def functorInstance: Functor[M0[Option, Int, Int, Int, Int, ?]] = ???
    val u = Unapply[Functor, M0[Option, Int, Int, Int, Int, String]]
    teq[u.M, M0[Option, Int, Int, Int, Int, ?]]
  }

  object unapply5 {
    class M0[F[_], A0, A1, B0]
    implicit def functorInstance: Functor[λ[α => M0[Option, α, α, String]]] = ???
    val u = Unapply[Functor, M0[Option, Int, Int, String]]
    teq[u.M, λ[α => M0[Option, α, α, String]]]
  }

  object unapply6 {
    class M0[F[_], A0, B0, C0]
    implicit def functorInstance: Functor[M0[Option, Int, Int, ?]] = ???
    val u = Unapply[Functor, M0[Option, Int, Int, String]]
    teq[u.M, M0[Option, Int, Int, ?]]
  }

  object unapply7 {
    class M0[F[_], A0, B0]
    implicit def functorInstance: Functor[M0[Option, ?, String]] = ???
    val u = Unapply[Functor, M0[Option, Int, String]]
    teq[u.M, M0[Option, ?, String]]
  }

  object unapply8 {
    class M0[F[_], A0, B0]
    implicit def functorInstance: Functor[M0[Option, Int, ?]] = ???
    val u = Unapply[Functor, M0[Option, Int, String]]
    teq[u.M, M0[Option, Int, ?]]
  }

  object unapply9 {
    class M0[F[_], A0]
    implicit def functorInstance: Functor[M0[Option, ?]] = ???
    val u = Unapply[Functor, M0[Option, String]]
    teq[u.M, M0[Option, ?]]
  }

  object unapply10 {
    class M0[A0, B0, C0, D0, E0, F0, G0]
    implicit def functorInstance: Functor[M0[Int, Int, Int, Int, Int, Int, ?]] = ???
    val u = Unapply[Functor, M0[Int, Int, Int, Int, Int, Int, String]]
    teq[u.M, M0[Int, Int, Int, Int, Int, Int, ?]]
  }

  object unapply11 {
    class M0[A0, B0, C0, D0, E0, F0]
    implicit def functorInstance: Functor[M0[Int, Int, Int, Int, Int, ?]] = ???
    val u = Unapply[Functor, M0[Int, Int, Int, Int, Int, String]]
    teq[u.M, M0[Int, Int, Int, Int, Int, ?]]
  }

  object unapply12 {
    class M0[A0, B0, C0, D0, E0]
    implicit def functorInstance: Functor[M0[Int, Int, Int, Int, ?]] = ???
    val u = Unapply[Functor, M0[Int, Int, Int, Int, String]]
    teq[u.M, M0[Int, Int, Int, Int, ?]]
  }

  object unapply13 {
    class M0[A0, B0, C0, D0]
    implicit def functorInstance: Functor[M0[Int, Int, Int, ?]] = ???
    val u = Unapply[Functor, M0[Int, Int, Int, String]]
    teq[u.M, M0[Int, Int, Int, ?]]
  }

  object unapply14 {
    class M0[A0, B0, C0]
    implicit def functorInstance: Functor[M0[Int, Int, ?]] = ???
    val u = Unapply[Functor, M0[Int, Int, String]]
    teq[u.M, M0[Int, Int, ?]]
  }

  object unapply15 {
    class M0[A0, B0]
    implicit def functorInstance: Functor[M0[Int, ?]] = ???
    val u = Unapply[Functor, M0[Int, String]]
    teq[u.M, M0[Int, ?]]
  }

  object unapply16 {
    class M0[A0, B0]
    implicit def functorInstance: Functor[M0[?, String]] = ???
    val u = Unapply[Functor, M0[Int, String]]
    teq[u.M, M0[?, String]]
  }

  object unapply17 {
    class MT[F[_], A]
    class MAB[A, B]
    implicit def functorInstance: Functor[MT[MAB[Int, ?], ?]] = ???
    val u = Unapply[Functor, MT[MAB[Int, ?], String]]
    teq[u.M, MT[MAB[Int, ?], ?]]
  }

  object unapply2_0 {
    class M0[F[_], A0, B0]
    implicit def bifunctorInstance: Bifunctor[M0[Option, ?, ?]] = ???
    val u = Unapply2[Bifunctor, M0[Option, Int, String]]
    teq2[u.M, M0[Option, ?, ?]]
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
