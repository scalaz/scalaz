package scalaz

trait Traverse[T[_]] {
  def traverse[F[_], A, B](f: A => F[B], t: T[A])(implicit a: Applicative[F]): F[T[B]]

  implicit val functor: Functor[T] = new Functor[T] {
    def fmap[A, B](k: T[A], f: A => B) = traverse[Identity, A, B](a => Identity.id(f(a)), k).value
  }
}

object Traverse {
  implicit val IdentityTraverse: Traverse[Identity] = new Traverse[Identity] {
    def traverse[F[_], A, B](f: A => F[B], t: Identity[A])(implicit a: Applicative[F]) = a.functor.fmap(f(t.value), Identity.id(_: B))
  }
}
