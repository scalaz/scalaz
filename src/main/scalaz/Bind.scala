package scalaz

trait Bind[Z[_]] {
  def bind[A, B](a: Z[A], f: A => Z[B]): Z[B]
}

object Bind {
  implicit val IdentityBind: Bind[Identity] = new Bind[Identity] {
    def bind[A, B](a: Identity[A], f: A => Identity[B]) = f(a.value)
  }

  implicit def ContinuationBind[R]: Bind[PartialApply1Of2[Continuation, R]#Apply] = new Bind[PartialApply1Of2[Continuation, R]#Apply] {
    def bind[A, B](a: Continuation[R, A], f: A => Continuation[R, B]) = Continuation.continuation[R, B](c => a(p => f(p)(c)))
  }

  implicit def OptionBind: Bind[Option] = new Bind[Option] {
    def bind[A, B](a: Option[A], f: A => Option[B]) = a flatMap f
  }
}
