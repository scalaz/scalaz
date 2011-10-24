package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Show` */
trait ShowV[F] extends SyntaxV[F] {
  implicit def F: Show[F]
  ////

  ////
}

trait ToShowSyntax  {
  implicit def ToShowV[F](v: F)(implicit F0: Show[F]) =
    new ShowV[F] { def self = v; implicit def F: Show[F] = F0 }

  ////

  ////
}

trait ShowSyntax[F]  {
  implicit def ToShowV(v: F)(implicit F0: Show[F]): ShowV[F] = new ShowV[F] { def self = v; implicit def F: Show[F] = F0 }

  ////

  ////
}
