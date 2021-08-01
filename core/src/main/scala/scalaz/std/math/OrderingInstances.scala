package scalaz
package std
package math

import scala.math.{Ordering => SOrdering}
import scalaz.Ordering


trait OrderingInstances {
  def orderingMonoid[A]: Monoid[SOrdering[A]] = new Monoid[SOrdering[A]] {
    def append(f1: SOrdering[A], f2: => SOrdering[A]) =
      (x: A, y: A) => f1.compare(x, y) match {
        case 0 => f2.compare(x, y)
        case o => o
      }
    def zero = (x: A, y: A) => 0
  }
}

trait OrderingFunctions {
  final def ToScalazOrderFromOrdering[A](oa: SOrdering[A]): scalaz.Order[A] =
    (x: A, y: A) => Ordering.fromInt(oa.compare(x, y))
}

object ordering extends OrderingInstances
