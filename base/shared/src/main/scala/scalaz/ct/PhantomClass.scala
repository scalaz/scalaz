package scalaz
package ct

trait PhantomClass[F[_]] extends FunctorClass[F] with ContravariantClass[F] {
  def pmap[A, B](ma: F[A]): F[B]
}

object PhantomClass {

  trait DeriveMapContramap[F[_]] extends PhantomClass[F] with Alt[DeriveMapContramap[F]] {
    final override def map[A, B](ma: F[A])(f: (A) => B): F[B]       = pmap(ma)
    final override def contramap[A, B](ma: F[A])(f: (B) => A): F[B] = pmap(ma)
  }

  trait DerivePmap[F[_]] extends PhantomClass[F] with Alt[DerivePmap[F]] {
    final override def pmap[A, B](ma: F[A]): F[B] = contramap(map(ma)(_ => ()))(_ => ())
  }

  trait Alt[D <: Alt[D]]
}
