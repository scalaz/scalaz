package scalaz
package typeclass

trait FunctorClass[F[_]] extends Functor[F]{
  final def functor: Functor[F] = this
}

object FunctorClass {
  trait Template[M[_]] extends Functor[M] with Map[M]

  trait Map[M[_]] { self: Functor[M] =>
    override def mapConst[A, B](ma: M[A])(c: B): M[B] = map(ma)(_ => c)
  }
}
