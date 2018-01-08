package scalaz.data

import scalaz.typeclass._

trait FreeInstances {

  def monad[F[_]]: MonadClass[Free[F, ?]] = new MonadClass.Template[Free[F, ?]] {
    override def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ma flatMap f
    override def ap[A, B](fa: Free[F, A])(f: Free[F, A => B]): Free[F, B] = f flatMap fa.map
    override def map[A, B](ma: Free[F, A])(f: A => B): Free[F, B] = ma map f
    override def pure[A](a: A): Free[F, A] = Free.pure(a)
  }
}
