package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.Prop.forAll
import scala.math.{Ordering => SOrdering}

class MapTest extends Spec {
  "map ordering" ! prop {
    val O = implicitly[Order[Map[String,Int]]]
    val O2 = SOrdering.Iterable(implicitly[SOrdering[(String,Int)]])
    (kvs: List[(String,Int)], kvs2: List[(String,Int)]) => {
      val (m1, m2) = (kvs.toMap, kvs2.toMap)
      if((m1.size == kvs.size) && (m2.size == kvs2.size)){
        val l: Boolean = O.lessThan(m1, m2)
        val r: Boolean = O2.lt(kvs.sortBy(_._1), kvs2.sortBy(_._1))
        l == r
      } else true
    }
  }
}
