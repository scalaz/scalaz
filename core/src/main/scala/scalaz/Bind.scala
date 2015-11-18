package scalaz

import scala.Predef.???

trait Bind[F[_]] {
  val functor: Functor[F]

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

  val apply: Apply[F] =
    new Apply[F] {
      val functor =
        Bind.this.functor

      def ap[A, B](f: F[A => B]): F[A] => F[B] =
        a => bd((ff: A => B) => bd((aa: A) => pure(ff(aa)))(a))(f)
    }


  def ap[A, B](f: F[A => B]): F[A] => F[B] =
    apply.ap(f)

  val applicative: Applicative[F]
  
  def pure[A]: A => F[A] =
    applicative.pure

  def bd[A, B](f: A => F[B]): F[A] => F[B]
}

object Bind {

}
