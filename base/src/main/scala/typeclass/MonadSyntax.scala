package scalaz
package typeclass

import scala.language.implicitConversions

trait MonadSyntax {
  implicit def monadOps[F[_], A](fa: F[A])(implicit F: Monad[F]): MonadSyntax.Ops[F, A] =
    new MonadSyntax.Ops(fa)
}

object MonadSyntax {
  class Ops[F[_], A](self: F[A])(implicit F: Monad[F]) {
    def map[B](f: A => B): F[B] = F.applicative.apply.functor.map[A, B](self)(f)
    def flatMap[B](f: A => F[B]): F[B] = F.bind.flatMap[A, B](self)(f) 
    def void: F[Unit] = F.applicative.apply.functor.map[A, Unit](self)(_ => ())
  }
}
