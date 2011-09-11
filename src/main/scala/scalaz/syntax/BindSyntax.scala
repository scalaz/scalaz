package scalaz
package syntax


trait BindV[F[_], A] extends SyntaxV[F[A]] {
  def flatMap[B](f: A => F[B])(implicit F: Bind[F]) = F.bind(self)(f)
}

trait JoinV[F[_], A] extends SyntaxV[F[F[A]]] {
  def join(implicit F: Bind[F]) = F.join(self)
}

trait ToBindSyntax extends ToApplySyntax {
  implicit def bind[F[_], A](v: F[A]) = (new BindSyntax[F] {}).bindV(v)
}

trait BindSyntax[F[_]] extends ApplySyntax[F] {
  implicit def bindV[A](v: F[A]) = new BindV[F, A] {
    def self = v
  }
}
