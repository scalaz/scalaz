package scalaz
package std

import std.AllInstances._
import org.scalacheck.Arbitrary, Arbitrary.arbitrary
import scalaz.scalacheck.ScalazProperties._
import scala.math.{Ordering => SOrdering}

class MapTest extends Spec {
  checkAll(traverse.laws[({type F[V] = Map[Int,V]})#F])
  checkAll(isEmpty.laws[({type F[V] = Map[Int,V]})#F])
  checkAll(monoid.laws[Map[Int,String]])
  checkAll(order.laws[Map[Int,String]])
  checkAll(equal.laws[Map[Int,String]])

  "satisfy equals laws when not natural" ! equal.laws[Map[NotNatural, String]]

  class NotNatural(val id: Int)
  implicit def NotNaturalArbitrary: Arbitrary[NotNatural] =
    Arbitrary(arbitrary[Int] map (new NotNatural(_)))

  implicit def NotNaturalOrder: Order[NotNatural] =
    Order.orderBy[NotNatural, Int](_.id)

  implicit def NotNaturalEqual: Equal[NotNatural] = new Equal[NotNatural] {
    def equal(a1: NotNatural, a2: NotNatural): Boolean = a1.id == a2.id
  }

  "map ordering" ! prop {
    val O = implicitly[Order[Map[String,Int]]]
    val O2 = SOrdering.Iterable(implicitly[SOrdering[(String,Int)]])
    (kvs: List[(String,Int)], kvs2: List[(String,Int)]) => {
      val (m1, m2) = (kvs.toMap, kvs2.toMap)
      ((m1.size == kvs.size) && (m2.size == kvs2.size)) ==> {
        val l: Boolean = O.lessThan(m1, m2)
        val r: Boolean = (if (m1.size < m2.size) true
                          else if (m1.size > m2.size) false
                          else O2.lt(kvs.sortBy(_._1), kvs2.sortBy(_._1)))
        l == r
      }
    }
  }
}
