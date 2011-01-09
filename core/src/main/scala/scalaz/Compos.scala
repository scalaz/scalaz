package scalaz


trait Compos[A] {
  def compos[F[_]:Applicative](a: A)(f: A => F[A]): F[A]
  def visit[F[_]](a: A)(f: PartialFunction[A,F[A]])(implicit F:Applicative[F]): F[A] =
    if (f isDefinedAt a) f(a)
    else F.pure(a)
}
