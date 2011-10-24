package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Equal` */
trait EqualV[F] extends SyntaxV[F] {
  implicit def F: Equal[F]
  ////

  ////
}

trait ToEqualSyntax  {
  implicit def ToEqualV[F](v: F)(implicit F0: Equal[F]) =
    new EqualV[F] { def self = v; implicit def F: Equal[F] = F0 }

  ////

  ////
}

trait EqualSyntax[F]  {
  implicit def ToEqualV(v: F)(implicit F0: Equal[F]): EqualV[F] = new EqualV[F] { def self = v; implicit def F: Equal[F] = F0 }

  ////

  ////
}
