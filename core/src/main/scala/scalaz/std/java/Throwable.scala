package scalaz
package std.java

trait ThrowableInstances {
  /** @since 7.0.1 */
  implicit val throwableInstance: Show[Throwable] = Show.show { t =>
    val clazz = Cord(t.getClass.getSimpleName)
    val sep = Cord(": ")
    val msg = Cord(t.getMessage)
    val l = t.getStackTrace.map(stacktraceelementShow.show).+:(clazz ++ sep ++ msg).toList

    std.list
      .listInstance
      .intercalate(l, Cord("\n"))
  }

  /** @since 7.2.28 */
  implicit val stacktraceelementShow: Show[StackTraceElement] = Show.showFromToString[StackTraceElement]
}

object throwable extends ThrowableInstances
