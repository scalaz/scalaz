package scalaz
package typeclass

trait PhantomClass[F[_]] extends Phantom[F] with Functor[F] with Contravariant[F] {
  final def phantom: Phantom[F] = this
}

object PhantomClass {
  trait Template[F[_]] extends PhantomClass[F] with MapContramap[F]

  trait MapContramap[F[_]] extends PhantomClass[F] {
    self: Phantom[F] with Functor[F] with Contravariant[F] =>
    override def map[A, B](ma: F[A])(f: (A) => B): F[B] = pmap(ma)

    override def contramap[A, B](ma: F[A])(f: (B) => A): F[B] = pmap(ma)
  }

  trait Pmap[F[_]] extends PhantomClass[F] {
    self: Phantom[F] with Functor[F] with Contravariant[F] =>
    override def pmap[A, B](ma: F[A]): F[B] = contramap(map(ma)(_ => ()))(_ => ())
  }
}

