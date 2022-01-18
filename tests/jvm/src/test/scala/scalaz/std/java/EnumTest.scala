package scalaz
package std.java


object EnumTest extends SpecLite {

  import syntax.`enum`._
  import std.java.`enum`._
  import java.util.concurrent.TimeUnit

  checkAll(scalaz.scalacheck.ScalazProperties.`enum`.laws[TimeUnit])

  //get all ordered 2-element subsets of TimeUnit
  @annotation.tailrec def pairs[A](l: List[A], acc: List[(A, A)] = Nil): List[(A, A)] = l match {
    case Nil | (_ :: Nil)      => acc
    case x :: (xxs @ (_ :: _)) => pairs(xxs, (xxs map (x -> _)) ::: acc)
  }

  "equal" in {
    TimeUnit.values forall (tu => tu === tu)
  }

  "not equal" in {
    pairs(TimeUnit.values.toList) forall { case (tu1, tu2) => tu1 =/= tu2 }
  }

  "min" in {
    Enum[TimeUnit].min exists { _ === TimeUnit.values.head }
  }

  "max" in {
    Enum[TimeUnit].max exists { _ === TimeUnit.values.last }
  }

  "pred" in {
    TimeUnit.values.sliding(2).map(_.toList) forall { case List(tu1, tu2) => tu1 === tu2.pred }
  }

  "succ" in {
    TimeUnit.values.sliding(2).map(_.toList) forall { case List(tu1, tu2) => tu1.succ === tu2 }
  }

  "order" in {
    pairs(TimeUnit.values.toList) forall { case (tu1, tu2) =>
        (Enum[TimeUnit].order(tu1, tu2) === Ordering.LT) &&
          (Enum[TimeUnit].order(tu2, tu1) === Ordering.GT) &&
          (Enum[TimeUnit].order(tu1, tu1) === Ordering.EQ) &&
          (Enum[TimeUnit].order(tu2, tu2) === Ordering.EQ)
    }
  }
}
