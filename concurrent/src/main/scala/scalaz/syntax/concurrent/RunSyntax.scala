package scalaz
package syntax
package concurrent

import scalaz.concurrent.Run

/** Wraps a value `self` and provides methods related to `Run` */
trait RunV[F] extends SyntaxV[F] {
  implicit def F: Run[F]
  ////

  ////
}

trait ToRunSyntax  {
  implicit def ToRunV[F](v: F)(implicit F0: Run[F]) =
    new RunV[F] { def self = v; implicit def F: Run[F] = F0 }

  ////

  ////
}

trait RunSyntax[F]  {
  implicit def ToRunV(v: F)(implicit F0: Run[F]): RunV[F] = new RunV[F] { def self = v; implicit def F: Run[F] = F0 }

  ////

  ////
}
