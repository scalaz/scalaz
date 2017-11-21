package scalaz
package example

import Prelude._

object DisjunctionExample extends App {
  import data.Disjunction._
  import data.Disjunction.Syntax._

  // Construction
  val l: String \/ Int = "foo".left
  val r: String \/ Int = 42.right

  // Pattern matching
  l match {
    case -\/(left) => println(left)
    case \/-(right) => println(right)
  }

  // Monad instance
  r.flatMap { right =>
    \/-(right)
  }
}
