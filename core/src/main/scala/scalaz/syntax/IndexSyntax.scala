package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Index` */
trait IndexV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Index[F]
  ////
  final def index(n: Int): Option[A] = F.index(self, n)
  final def indexOr(default: => A, n: Int): A = F.indexOr(self, default, n)
  ////
}

trait ToIndexV  {
  implicit def ToIndexV[FA](v: FA)(implicit F0: Unapply[Index, FA]) =
    new IndexV[F0.M,F0.A] { def self = F0(v); implicit def F: Index[F0.M] = F0.TC }

  ////

  ////
}

trait IndexSyntax[F[_]]  {
  implicit def ToIndexV[A](v: F[A])(implicit F0: Index[F]): IndexV[F, A] = new IndexV[F,A] { def self = v; implicit def F: Index[F] = F0 }

  ////

  ////
}
