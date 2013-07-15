package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadPlus` */
sealed abstract class MonadPlusOps[F[_],A] extends Ops[F[A]] {
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

trait ToMonadPlusOps0 {
  implicit def ToMonadPlusOpsUnapply[FA](v: FA)(implicit F0: Unapply[MonadPlus, FA]) =
    new MonadPlusOps[F0.M,F0.A] { def self = F0(v); implicit def F: MonadPlus[F0.M] = F0.TC }

}

trait ToMonadPlusOps extends ToMonadPlusOps0 with ToMonadOps with ToApplicativePlusOps {
  implicit def ToMonadPlusOps[F[_],A](v: F[A])(implicit F0: MonadPlus[F]) =
    new MonadPlusOps[F,A] { def self = v; implicit def F: MonadPlus[F] = F0 }

  ////

  ////
}

trait MonadPlusSyntax[F[_]] extends MonadSyntax[F] with ApplicativePlusSyntax[F] {
  implicit def ToMonadPlusOps[A](v: F[A]): MonadPlusOps[F, A] = new MonadPlusOps[F,A] { def self = v; implicit def F: MonadPlus[F] = MonadPlusSyntax.this.F }

  def F: MonadPlus[F]
  ////

  ////
}
