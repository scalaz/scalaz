package scalaz
package effect

import std.effect.autoCloseable._

object ResourceTestJVM extends SpecLite {

  // scala-js does not have java.sql classes
  // https://github.com/scala-js/scala-js/tree/v0.6.14/javalib/src/main/scala/java
  object instances {
    Resource[java.sql.Connection]
    Resource[java.sql.PreparedStatement]
    Resource[java.sql.ResultSet]
    Resource[java.sql.Statement]
  }

}
