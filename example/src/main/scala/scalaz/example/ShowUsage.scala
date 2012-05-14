package scalaz.example

import scalaz._
import Scalaz._

object ShowUsage extends App {

  /* ***************************************************************************
     Use a `Show`
     ************************************************************************ */

  /* You can call show using object-oriented syntax. */
  assert(1.show === "1".toList)
  assert("hello, world".show === "hello, world".toList)

  /* Instead of getting a Java-unfriendly `List[Char]`, let's get a `String` */
  assert(1.shows === "1")
  assert("string".shows === "string")

  /* As we just saw, Scalaz provides `Show` instances for `Int` and `String`.
    In fact, Scalaz can show ...TODO... out of the box. */ 

  /* Print a `Show` to stdout. NOT PURE. */
  "side effect".print
  "side effect newline".println

  /* ***************************************************************************
     Create your own `Show`s
     ************************************************************************ */

  case class Apple(juicy: Boolean)
  val apple = Apple(true)

  /* Case classes come with a reasonable `toString`, so use it as a `Show` */
  object AppleShow1 {
    implicit def appleShow: Show[Apple] = Show.showFromToString[Apple]
    assert(apple.show === "Apple(true)".toList)
  }

  /* Completely custom `Show` using convenience function. */
  object AppleShow2 {
    implicit def appleShow: Show[Apple] = Show.show {
      case Apple(true) => "juicy apple".toList
      case Apple(false) => "sour apple".toList
    }
    assert(apple.show === "juicy apple".toList)
  }

  /* Completely custom `Show` not using convenience function. */
  object AppleShow3 {
    implicit object AppleIsShow extends Show[Apple] {
      override def show(apple: Apple): List[Char] = apple match {
        case Apple(true) => "juicy apple".toList
        case Apple(false) => "sour apple".toList
      }
    }
    assert(apple.show === "juicy apple".toList)
  }

  //TODO Show.showContravariant
}
