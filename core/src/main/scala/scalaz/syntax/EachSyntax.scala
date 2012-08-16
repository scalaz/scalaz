package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Each` */
trait EachOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Each[F]
  ////
  final def foreach(f: A => Unit): Unit = F.each(self)(f)
  ////
}

trait ToEachOps0 {
  implicit def ToEachOpsUnapply[FA](v: FA)(implicit F0: Unapply[Each, FA]) =
    new EachOps[F0.M,F0.A] { def self = F0(v); implicit def F: Each[F0.M] = F0.TC }

}

trait ToEachOps extends ToEachOps0 {
  implicit def ToEachOps[F[_],A](v: F[A])(implicit F0: Each[F]) =
    new EachOps[F,A] { def self = v; implicit def F: Each[F] = F0 }

  ////

  ////
}

trait EachSyntax[F[_]]  { self => 
  implicit def ToEachOps[A](v: F[A]): EachOps[F, A] = new EachOps[F,A] { def self = v; implicit def F: Each[F] = self.F }

  def F: Each[F]
  ////

  ////
}
