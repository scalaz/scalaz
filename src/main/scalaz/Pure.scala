package scalaz

trait Pure[P[_]] {
  implicit def pure[A](a: A): P[A]
}

object Pure {
  implicit val IdentityPure = new Pure[Identity] {
    def pure[A](a: A) = Identity.id(a)
  }

  implicit def ContinuationPure[R] = new Pure[PartialApply1Of2[Continuation, R]#Apply] {
    def pure[A](a: A) = Continuation.continuation[R, A](_(a))
  }

  implicit def OptionPure = new Pure[Option] {
    def pure[A](a: A) = Some(a)
  }
}
