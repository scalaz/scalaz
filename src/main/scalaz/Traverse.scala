package scalaz

trait Traverse[T[_]] {
  def traverse[F[_], A, B](f: A => F[B], t: T[A])(implicit a: Applicative[F]): F[T[B]]
}

object Traverse {
  implicit val IdentityTraverse: Traverse[Identity] = new Traverse[Identity] {
    def traverse[F[_], A, B](f: A => F[B], t: Identity[A])(implicit a: Applicative[F]) = a.functor.fmap(f(t.value), Identity.id(_: B))
  }
}
