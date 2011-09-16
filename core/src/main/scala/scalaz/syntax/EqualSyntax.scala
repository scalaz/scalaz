package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Equal` */
trait EqualV[F] extends SyntaxV[F] {
  ////

  ////
}

trait ToEqualSyntax  {
  implicit def equal[F](v: F) =
    (new EqualSyntax[F] {}).equalV(v)
}

trait EqualSyntax[F]  {
  implicit def equalV(v: F): EqualV[F] = new EqualV[F] { def self = v }

  ////

  ////
}
