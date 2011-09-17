package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Semigroup` */
trait SemigroupV[F] extends SyntaxV[F] {
  ////

  ////
}

trait ToSemigroupSyntax  {
  implicit def ToSemigroupV[F](v: F) =
    (new SemigroupSyntax[F] {}).ToSemigroupV(v)

  ////

  ////
}

trait SemigroupSyntax[F]  {
  implicit def ToSemigroupV(v: F): SemigroupV[F] = new SemigroupV[F] { def self = v }

  ////

  ////
}
