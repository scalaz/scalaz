package scalaz
package data

import scalaz.typeclass.MonadClass

trait FreerInstances {

  implicit def monad[F[_]]: MonadClass[Freer[F, ?]] = new MonadClass.Template[Freer[F, ?]] {
    override def flatMap[A, B](ma: Freer[F, A])(f: A => Freer[F, B]): Freer[F, B] = ma flatMap f
    override def ap[A, B](fa: Freer[F, A])(f: Freer[F, A => B]): Freer[F, B] = flatMap(f)(map(fa))
    override def pure[A](a: A): Freer[F, A] = Freer.pure(a)
  }
}
