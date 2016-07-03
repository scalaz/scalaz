package scalaz
package typeclass

trait BindClass[F[_]] extends Bind[F] with ApplyClass[F] {
  final def bind: Bind[F] = this
}

object BindClass {
  trait Template[F[_]] extends BindClass[F] with Ap[F] with Bind.Flatten[F]

  trait AltTemplate[F[_]] extends BindClass[F] with Ap[F] with Bind.FlatMap[F]

  trait Ap[F[_]] { self: BindClass[F] =>
    override def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(functor.map(fa))
  }

}
