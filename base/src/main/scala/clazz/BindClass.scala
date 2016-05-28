package scalaz
package clazz

trait BindClass[F[_]] extends Bind[F] with ApplyClass[F] {
  implicit final def bind: Bind[F] = this
}

object BindClass {
  trait Template[F[_]] extends BindClass[F] with Ap[F]

  trait Ap[F[_]] extends Apply[F] { self: Bind[F] =>
    override final def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(functor.map(fa))
  }
}
