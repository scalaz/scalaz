package scalaz
package data

import typeclass._

trait AMaybeInstances {

  implicit final def show[F[_, _], A, B](implicit FAB: Show[F[A, B]]): Show[AMaybe[F, A, B]] = {
    case AJust(value) => s"AMaybe(${FAB.show(value)})"
    case AEmpty()     =>  "AEmpty()"
  }

}
