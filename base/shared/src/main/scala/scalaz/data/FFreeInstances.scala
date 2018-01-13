package scalaz.data

import scalaz.typeclass.MonadClass

trait FFreeInstances {

  implicit def monadFree[F[_]]: MonadClass[FFree[F, ?]] = new MonadClass.Template[FFree[F, ?]] {
    override def flatMap[A, B](ma: FFree[F, A])(f: A => FFree[F, B]): FFree[F, B] = ma flatMap f
    override def ap[A, B](fa: FFree[F, A])(f: FFree[F, A => B]): FFree[F, B] = flatMap(f)(map(fa))
    override def pure[A](a: A): FFree[F, A] = FFree.pure(a)
  }
}
