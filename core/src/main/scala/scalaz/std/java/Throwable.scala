package scalaz
package std.java

trait ThrowableInstances {
  implicit val throwableInstance: Show[Throwable] = Show.showFromToString[Throwable]
}

object throwable extends ThrowableInstances
