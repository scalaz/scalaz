package scalaz
package data

trait AMaybeInstances {
  implicit final def amaybeShow[F[_, _], A, B](implicit FAB: Show[F[A, B]]): Show[AMaybe[F, A, B]] = {
    case AJust(value) => s"AMaybe(${FAB.show(value)})"
    case AEmpty()     =>  "AEmpty()"
  }

}
