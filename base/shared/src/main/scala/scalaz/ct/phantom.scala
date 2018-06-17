package scalaz
package ct

import scala.language.experimental.macros

trait PhantomClass[F[_]] extends FunctorClass[F] with ContravariantClass[F] {

  def pmap[A, B](ma: F[A]): F[B]

  override def imap[A, B](ma: F[A])(f: A => B)(g: B => A): F[B] = pmap(ma)
}

object PhantomClass {

  trait DeriveMapContramap[F[_]] extends PhantomClass[F] with Alt[DeriveMapContramap[F]] {
    final override def map[A, B](ma: F[A])(f: (A) => B): F[B]       = pmap(ma)
    final override def contramap[A, B](ma: F[A])(f: (B) => A): F[B] = pmap(ma)
  }

  trait DerivePmap[F[_]] extends PhantomClass[F] with Alt[DerivePmap[F]] {
    final override def pmap[A, B](ma: F[A]): F[B] = contramap(map(ma)(_ => ()))(_ => ())
  }

  trait Alt[D <: Alt[D]]
}

trait PhantomFunctions {
  def pmap[F[_], A, B](fa: F[A])(implicit F: Phantom[F]): F[B] = F.pmap(fa)
}

trait PhantomSyntax {
  implicit final class ToPhantomOps[F[_], A](self: F[A]) {
    def pmap[B](implicit ev: Phantom[F]): F[B] = macro meta.Ops.i_0
  }
}
