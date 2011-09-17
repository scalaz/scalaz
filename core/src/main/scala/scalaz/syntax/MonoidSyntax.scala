package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Monoid` */
trait MonoidV[F] extends SyntaxV[F] {
  ////

  ////
}

trait ToMonoidSyntax  {
  implicit def monoid[F](v: F) =
    (new MonoidSyntax[F] {}).monoidV(v)

  ////

  ////
}

trait MonoidSyntax[F]  {
  implicit def monoidV(v: F): MonoidV[F] = new MonoidV[F] { def self = v }

  ////

  ////
}
