package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `CoBind` */
trait CoBindV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: CoBind[F]
  ////
  def cobind[B](f: F[A] => B) = F.cobind(self)(f)
  ////
}

trait ToCoBindV  {
  implicit def ToCoBindV[FA](v: FA)(implicit F0: Unapply[CoBind, FA]) =
    new CoBindV[F0.M,F0.A] { def self = F0(v); implicit def F: CoBind[F0.M] = F0.TC }

  ////

  ////
}

trait CoBindSyntax[F[_]]  {
  implicit def ToCoBindV[A](v: F[A])(implicit F0: CoBind[F]): CoBindV[F, A] = new CoBindV[F,A] { def self = v; implicit def F: CoBind[F] = F0 }

  ////

  ////
}
