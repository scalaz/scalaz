package scalaz
package data

import scalaz.debug.DebugClass

trait AMaybeInstances {
  implicit final def amaybeDebug[F[_, _], A, B](implicit FAB: Debug[F[A, B]]): Debug[AMaybe[F, A, B]] =
    instanceOf[DebugClass[AMaybe[F, A, B]]] {
      case AJust(value) => s"AMaybe(${FAB.debug(value)})"
      case AEmpty()     => "AEmpty()"
    }

}
