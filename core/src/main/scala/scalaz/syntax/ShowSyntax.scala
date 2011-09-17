package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Show` */
trait ShowV[F] extends SyntaxV[F] {
  ////

  ////
}

trait ToShowSyntax  {
  implicit def ToShowV[F](v: F) =
    (new ShowSyntax[F] {}).ToShowV(v)

  ////

  ////
}

trait ShowSyntax[F]  {
  implicit def ToShowV(v: F): ShowV[F] = new ShowV[F] { def self = v }

  ////

  ////
}
