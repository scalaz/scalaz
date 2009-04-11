package scalaz

sealed trait Pointed[P[_]] {
  val functor: Functor[P]
  val pure: Pure[P]
}

object Pointed {
  def pointed[P[_]](implicit f: Functor[P], p: Pure[P]) = new Pointed[P] {
    val functor = f
    val pure = p
  }

  implicit val IdentityPointed = pointed[Identity]

  implicit def ContinuationPointed[R] = pointed[PartialApply1Of2[Continuation, R]#Apply]

  implicit val OptionPointed = pointed[Option]
}
