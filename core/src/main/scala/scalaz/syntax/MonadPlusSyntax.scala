package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadPlus` */
trait MonadPlusV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: MonadPlus[F]
  ////
  import Liskov._

  def filter(f: A => Boolean) =
    F.filter(self)(f)
  
  def withFilter(f: A => Boolean) =
    filter(f)

  def unite[T[_], B](implicit ev: A <~< T[B], T: Foldable[T]): F[B] = {
    val ftb: F[T[B]] = Liskov.co[F, A, T[B]](ev)(self)
    F.unite[T, B](ftb)
  }

  ////
}

trait ToMonadPlusV0 {
  implicit def ToMonadPlusVUnapply[FA](v: FA)(implicit F0: Unapply[MonadPlus, FA]) =
    new MonadPlusV[F0.M,F0.A] { def self = F0(v); implicit def F: MonadPlus[F0.M] = F0.TC }

}

trait ToMonadPlusV extends ToMonadPlusV0 with ToMonadV with ToApplicativePlusV {
  implicit def ToMonadPlusV[F[_],A](v: F[A])(implicit F0: MonadPlus[F]) =
    new MonadPlusV[F,A] { def self = v; implicit def F: MonadPlus[F] = F0 }

  ////

  ////
}

trait MonadPlusSyntax[F[_]] extends MonadSyntax[F] with ApplicativePlusSyntax[F] {
  implicit def ToMonadPlusV[A](v: F[A])(implicit F0: MonadPlus[F]): MonadPlusV[F, A] = new MonadPlusV[F,A] { def self = v; implicit def F: MonadPlus[F] = F0 }

  ////

  ////
}
