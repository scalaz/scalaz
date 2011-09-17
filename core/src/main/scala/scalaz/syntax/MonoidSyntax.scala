package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Monoid` */
trait MonoidV[F] extends SyntaxV[F] {
  ////

  ////
}

trait ToMonoidSyntax  {
  implicit def ToMonoidV[F](v: F) =
    (new MonoidSyntax[F] {}).ToMonoidV(v)

  ////

  ////
}

trait MonoidSyntax[F]  {
  implicit def ToMonoidV(v: F): MonoidV[F] = new MonoidV[F] { def self = v }

  ////

  ////
}
