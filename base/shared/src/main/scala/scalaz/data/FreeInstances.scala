package scalaz
package data

import scalaz.typeclass._

trait FreeInstances {

  implicit final def monad[F[_]]: Monad[Free[F, ?]] =
    instanceOf(new MonadClass[Free[F, ?]] {
      override def map[A, B](ma: Free[F, A])(f: A => B): Free[F, B]              = ma map f
      override def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ma flatMap f
      override def flatten[A](ma: Free[F, Free[F, A]]): Free[F, A]               = flatMap(ma)(a => a)
      override def ap[A, B](fa: Free[F, A])(f: Free[F, A => B]): Free[F, B]      = f flatMap fa.map
      override def pure[A](a: A): Free[F, A]                                     = Free.pure(a)
    })
}
