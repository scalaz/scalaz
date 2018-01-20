package scalaz
package data

trait AMaybeInstances {
  implicit final def amaybeDebug[F[_, _], A, B](implicit FAB: Debug[F[A, B]]): Debug[AMaybe[F, A, B]] = {
    case AJust(value) => s"AMaybe(${FAB.debug(value)})"
    case AEmpty()     =>  "AEmpty()"
  }

}
