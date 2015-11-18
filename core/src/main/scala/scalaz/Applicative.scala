package scalaz

import scala.Function1

trait Applicative[F[_]] {
  val functor: Functor[F] =
    new Functor[F] {
      val functor =
        new GeneralFunctor[Function1, Function1, F] {
          override def fmap[A, B](f: A => B): F[A] => F[B] =      
            apply.ap(pure(f))
        }

    }

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

  val apply: Apply[F]

  def pure[A]: A => F[A]
}

object Applicative {

}
