package scalaz
package tc

import scala.language.experimental.macros

@meta.minimal("pmap", ("map", "contramap"))
trait PhantomClass[F[_]] extends FunctorClass[F] with ContravariantClass[F] {
  def pmap[A, B](ma: F[A]): F[B] = contramap(map(ma)(_ => ()))(_ => ())

  override def map[A, B](ma: F[A])(f: (A) => B): F[B] = pmap(ma)

  override def contramap[A, B](ma: F[A])(f: (B) => A): F[B] = pmap(ma)

  override def imap[A, B](ma: F[A])(f: A => B)(g: B => A): F[B] = pmap(ma)
}

trait PhantomFunctions {
  def pmap[F[_], A, B](fa: F[A])(implicit F: Phantom[F]): F[B] = F.pmap(fa)
}

trait PhantomSyntax {
  implicit final class ToPhantomOps[F[_], A](self: F[A]) {
    def pmap[B](implicit ev: Phantom[F]): F[B] = macro ops.Ops.i_0
  }
}
