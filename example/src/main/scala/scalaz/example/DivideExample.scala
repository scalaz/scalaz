package scalaz
package example

import scalaz.std.anyVal._
import scalaz.std.string._

object DivideExample {

  final case class User(id: Int, name: String)

  object User {
    implicit val instance: Order[User] =
      Divide[Order].deriving2(Function.unlift(unapply))
  }

}
