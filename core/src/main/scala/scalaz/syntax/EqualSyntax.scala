package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Equal` */
trait EqualV[F] extends SyntaxV[F] {
  ////

  ////
}

trait ToEqualSyntax  {
  implicit def ToEqualV[F](v: F) =
    new EqualV[F] { def self = v }

  ////

  ////
}

trait EqualSyntax[F]  {
  implicit def ToEqualV(v: F): EqualV[F] = new EqualV[F] { def self = v }

  ////

  ////
}
