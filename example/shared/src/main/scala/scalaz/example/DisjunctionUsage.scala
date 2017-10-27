package scalaz.example

import scalaz.data.Disjunction
import Disjunction._

object DisjunctionUsage extends App {
  val left: String \/ Int = -\/("Foo")
  val right: String \/ Int = \/-(42)
  val disj: String \/ Int = Disjunction.fromEither(Left("Foo"))

  // Pattern match
  left match {
    case -\/(str) => println(str)
    case \/-(i) => println(i)
  }

  // Fold
  val folded: String = disj.fold(_ + "Bar")(_.toString)

  // try/catch
  val caught: Throwable \/ String = Disjunction.fromTryCatchNonFatal {
    null.toString
  }
}
