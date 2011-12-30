package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Each` */
trait EachV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Each[F]
  ////
  final def foreach(f: A => Unit): Unit = F.each(self)(f)
  ////
}

trait ToEachV0 {
  implicit def ToEachVUnapply[FA](v: FA)(implicit F0: Unapply[Each, FA]) =
    new EachV[F0.M,F0.A] { def self = F0(v); implicit def F: Each[F0.M] = F0.TC }

}

trait ToEachV extends ToEachV0 {
  implicit def ToEachV[F[_],A](v: F[A])(implicit F0: Each[F]) =
    new EachV[F,A] { def self = v; implicit def F: Each[F] = F0 }

  ////

  ////
}

trait EachSyntax[F[_]]  {
  implicit def ToEachV[A](v: F[A])(implicit F0: Each[F]): EachV[F, A] = new EachV[F,A] { def self = v; implicit def F: Each[F] = F0 }

  ////

  ////
}
