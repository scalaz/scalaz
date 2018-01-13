package scalaz.data

import scalaz.Monad
import scalaz.typeclass.MonadClass

trait CodensityInstances {
  implicit def monad[M[_]](implicit M: Monad[M]): MonadClass[Codensity[M, ?]] = new MonadClass.Template[Codensity[M, ?]] {
    override def pure[A](a: A): Codensity[M, A] = Codensity.rep(M.applicative.pure(a))
    override def ap[A, B](fa: Codensity[M, A])(f: Codensity[M, A => B]): Codensity[M, B] = flatMap(f)(map(fa))
    override def flatMap[A, B](ma: Codensity[M, A])(f: A => Codensity[M, B]): Codensity[M, B] =
      Codensity.rep(M.bind.flatMap(Codensity.abs(ma))(a => Codensity.abs(f(a))))
  }
}
