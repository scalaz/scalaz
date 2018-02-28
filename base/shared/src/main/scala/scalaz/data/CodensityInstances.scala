package scalaz
package data

import scalaz.typeclass.MonadClass

trait CodensityInstances {
  implicit def monad[M[_]](implicit M: Monad[M]): Monad[Codensity[M, ?]] =
    instanceOf(new MonadClass[Codensity[M, ?]] {
      override def map[A, B](ma: Codensity[M, A])(f: A => B): Codensity[M, B] =
        Codensity.rep(M.map(Codensity.abs(ma))(f))
      override def pure[A](a: A): Codensity[M, A]                                          = Codensity.rep(M.pure(a))
      override def ap[A, B](fa: Codensity[M, A])(f: Codensity[M, A => B]): Codensity[M, B] = flatMap(f)(map(fa))

      override def flatten[A](ma: Codensity[M, Codensity[M, A]]): Codensity[M, A] = flatMap(ma)(a => a)
      override def flatMap[A, B](ma: Codensity[M, A])(f: A => Codensity[M, B]): Codensity[M, B] =
        Codensity.rep(M.flatMap(Codensity.abs(ma))(a => Codensity.abs(f(a))))
    })
}
