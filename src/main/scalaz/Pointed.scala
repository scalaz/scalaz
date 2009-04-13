package scalaz

trait Pointed[P[_]] extends Functor[P] with Pure[P]

object Pointed {
  def pointed[P[_]](implicit t: Functor[P], p: Pure[P]) = new Pointed[P] {
    def fmap[A, B](a: P[A], f: A => B) = t.fmap(a, f)
    def pure[A](a: A): P[A] = p.pure(a)
  }

  implicit val IdentityPointed = pointed[Identity]

  implicit def ContinuationPointed[R] = pointed[PartialApply1Of2[Continuation, R]#Apply]

  implicit val OptionPointed = pointed[Option]
}
