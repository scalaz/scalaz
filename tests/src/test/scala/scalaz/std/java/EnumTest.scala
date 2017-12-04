package scalaz
package std.java


object EnumTest extends SpecLite {

  import syntax.enum._
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

  "min" in {
    Enum[TimeUnit].min exists { _ === TimeUnit.values.head }
  }

  "max" in {
    Enum[TimeUnit].max exists { _ === TimeUnit.values.last }
  }

  "pred" in {
    TimeUnit.values.sliding(2) forall { case Array(tu1, tu2) => tu1 === tu2.pred }
  }

  "succ" in {
    TimeUnit.values.sliding(2) forall { case Array(tu1, tu2) => tu1.succ === tu2 }
  }

  "order" in {
    //get all ordered 3-elements subsets of TimeUnit
    @annotation.tailrec def triplets[A](xs: List[A], xlts: List[A] = Nil, acc: List[(A, A, A)] = Nil): List[(A, A, A)] = xs match {
      case Nil => acc
      case x :: xgts => triplets(xgts, x :: xlts, (for (xlt <- xlts; xgt <- xgts) yield (xlt, x, xgt)) ::: acc)
    }
    triplets(TimeUnit.values.toList) forall {
      case (tult, tu, tugt) =>
        (Enum[TimeUnit].order(tult, tu) === Ordering.LT) &&
          (Enum[TimeUnit].order(tu, tult) === Ordering.GT) &&
          (Enum[TimeUnit].order(tu, tu) === Ordering.EQ) &&
          (Enum[TimeUnit].order(tugt, tu) === Ordering.GT) &&
          (Enum[TimeUnit].order(tu, tugt) === Ordering.LT)
    }
  }
}
