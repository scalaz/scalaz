package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Show` */
trait ShowV[F] extends SyntaxV[F] {
  ////

  ////
}

trait ToShowSyntax  {
  implicit def show[F](v: F) =
    (new ShowSyntax[F] {}).showV(v)

  ////

  ////
}

trait ShowSyntax[F]  {
  implicit def showV(v: F): ShowV[F] = new ShowV[F] { def self = v }

  ////

  ////
}
