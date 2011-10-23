package scalaz
package syntax
package concurrent

/** Wraps a value `self` and provides methods related to `Run` */
trait RunV[F] extends SyntaxV[F] {
  ////

  ////
}

trait ToRunSyntax  {
  implicit def ToRunV[F](v: F) =
    new RunV[F] { def self = v }

  ////

  ////
}

trait RunSyntax[F]  {
  implicit def ToRunV(v: F): RunV[F] = new RunV[F] { def self = v }

  ////

  ////
}
