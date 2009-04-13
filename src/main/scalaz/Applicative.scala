package scalaz

trait Applicative[Z[_]] extends Pointed[Z] with Apply[Z]

object Applicative {
  def applicative[Z[_]](implicit p: Pure[Z], a: Apply[Z]) = new Applicative[Z] {
    def fmap[A, B](fa: Z[A], f: A => B) = a(p.pure(f), fa)
    def pure[A](a: A) = p.pure(a)
    def apply[A, B](f: Z[A => B], x: Z[A]) = a(f, x)
  }

  implicit val IdentityApplicative = applicative[Identity]

  implicit def ContinuationApplicative[R] = applicative[PartialApply1Of2[Continuation, R]#Apply]

  implicit def OptionApplicative = applicative[Option]
}
