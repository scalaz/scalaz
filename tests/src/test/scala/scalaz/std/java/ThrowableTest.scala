package scalaz
package std.java

import std.AllInstances._
import throwable._

object ThrowableTest extends SpecLite {
  "show" in {
    import syntax.show._
    val msg = "test123"
    val t = new Throwable(msg)
    val s = t.shows

    s.startsWith(s"Throwable: ${msg}") must_=== (true)
    s.contains(this.getClass.getSimpleName) must_=== (true)
  }
}