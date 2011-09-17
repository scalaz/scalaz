package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Semigroup` */
trait SemigroupV[F] extends SyntaxV[F] {
  ////

  ////
}

trait ToSemigroupSyntax  {
  implicit def semigroup[F](v: F) =
    (new SemigroupSyntax[F] {}).semigroupV(v)

  ////

  ////
}

trait SemigroupSyntax[F]  {
  implicit def semigroupV(v: F): SemigroupV[F] = new SemigroupV[F] { def self = v }

  ////

  ////
}
