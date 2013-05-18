package scalaz
package std.java

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class EnumTest extends Spec {

  import syntax.equal._
  import std.java.enum._
  import java.util.concurrent.TimeUnit

  "equal" in {
    TimeUnit.values forall (tu => tu === tu)
  }

  "not equal" in {
    //get all 2-element subsets of TimeUnit
    @annotation.tailrec def pairs[A](l: List[A], acc: List[(A, A)] = Nil): List[(A, A)] = l match {
      case Nil | (_ :: Nil)      => acc
      case x :: (xxs @ (_ :: _)) => pairs(xxs, (xxs map (x -> _)) ::: acc)
    }
    pairs(TimeUnit.values.toList) forall { case (tu1, tu2) => tu1 =/= tu2 }
  }

}
