package scalaz
package std.java

trait ThrowableInstances {
  /** @since 7.0.1 */
  implicit val throwableInstance: Show[Throwable] = Show.showFromToString[Throwable]
}

object throwable extends ThrowableInstances
