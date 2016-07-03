package scalaz
package typeclass

trait Bind[M[_]] {
  def apply: Apply[M]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def flatten[A](ma: M[M[A]]): M[A]
}

object Bind extends BindInstances with BindFunctions {

  trait FlatMap[F[_]] extends FlatMapXorFlatten[FlatMap[F]] { self: Bind[F] =>
    override def flatMap[A, B](ma: F[A])(f: (A) => F[B]): F[B] = flatten(apply.functor.map(ma)(f))
  }
  trait Flatten[F[_]] extends FlatMapXorFlatten[Flatten[F]] { self: Bind[F] =>
    override def flatten[A](ma: F[F[A]]): F[A] = flatMap(ma)(identity)
  }
  trait FlatMapXorFlatten[D <: FlatMapXorFlatten[D]]

  def apply[F[_]](implicit F: Bind[F]): Bind[F] = F

  object syntax extends BindSyntax
}
