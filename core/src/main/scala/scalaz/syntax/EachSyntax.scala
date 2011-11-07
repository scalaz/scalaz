package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Each` */
trait EachV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Each[F]
  ////
  final def foreach(f: A => Unit): Unit = F.each(self)(f)
  ////
}

trait ToEachV  {
  implicit def ToEachV[FA](v: FA)(implicit F0: Unapply[Each, FA]) =
    new EachV[F0.M,F0.A] { def self = F0(v); implicit def F: Each[F0.M] = F0.TC }

  ////

  ////
}

trait EachSyntax[F[_]]  {
  implicit def ToEachV[A](v: F[A])(implicit F0: Each[F]): EachV[F, A] = new EachV[F,A] { def self = v; implicit def F: Each[F] = F0 }

  ////

  ////
}
