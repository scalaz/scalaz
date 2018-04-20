package scalaz
package std

import std.AllInstances._
import std.sortedMap._
import org.scalacheck.Arbitrary, Arbitrary.arbitrary
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scala.collection.immutable.{Map => SMap, SortedMap}
import scala.math.{Ordering => SOrdering}
import org.scalacheck.Prop.forAll

object SortedMapTest extends SpecLite {

  checkAll(traverse.laws[SortedMap[Int, ?]])
  checkAll(FoldableTests.anyAndAllLazy[SortedMap[Int, ?]])
  checkAll(isEmpty.laws[SortedMap[Int, ?]])
  checkAll(bind.laws[SortedMap[Int, ?]])
  checkAll(align.laws[SortedMap[Int, ?]])
  checkAll(monoid.laws[SortedMap[Int,String]])
  checkAll(order.laws[SortedMap[Int,String]])
  checkAll(band.laws[SortedMap[String, ISet[Int]]])

  checkAll("satisfy equals laws when not natural", equal.laws[SortedMap[NotNatural, String]])

  implicit def sortedMapArb[A: Arbitrary: Order, B: Arbitrary]: Arbitrary[SortedMap[A, B]] = {
    implicit val o = Order[A].toScalaOrdering
    Arbitrary(arbitrary[SMap[A, B]] map (m => SortedMap(m.toSeq:_*)))
  }

  "SortedMap ordering" ! forAll {
    val O = implicitly[Order[SortedMap[String,Int]]]
    val O2 = SOrdering.Iterable(implicitly[SOrdering[(String,Int)]])
    (kvs: List[(String,Int)], kvs2: List[(String,Int)]) => {
      val (m1, m2) = (SortedMap(kvs:_*), SortedMap(kvs2:_*))
      ((m1.size == kvs.size) && (m2.size == kvs2.size)) ==> {
        val l: Boolean = O.lessThan(m1, m2)
        val r: Boolean = (if (m1.size < m2.size) true
                          else if (m1.size > m2.size) false
                          else O2.lt(kvs.sortBy(_._1), kvs2.sortBy(_._1)))
        l == r
      }
    }
  }

  "align" ! forAll { (a: SortedMap[Int, String], b: SortedMap[Int, Long]) =>
    import std.set._, \&/._
    val F = Align[SortedMap[Int, ?]]
    val x = F.align(a, b)
    val keysA = a.keySet
    val keysB = b.keySet

    x must_=== F.alignWith[String, Long, String \&/ Long](identity)(a, b)
    ==>>.fromList(x.toList) must_=== Align[Int ==>> ?].align(==>>.fromList(a.toList), ==>>.fromList(b.toList))
    x.keySet must_== (keysA ++ keysB)

    x.filter(_._2.isThis).keySet must_== (keysA diff keysB)
    x.filter(_._2.isThat).keySet must_== (keysB diff keysA)
    x.filter(_._2.isBoth).keySet must_== (keysA & keysB)

    x.filter(_._2.isThis) must_=== F.map(a.filter{case (k, _) => ! keysB(k)})(This(_))
    x.filter(_._2.isThat) must_=== F.map(b.filter{case (k, _) => ! keysA(k)})(That(_))
  }

  "getOrAdd" ! forAll { (m0: SortedMap[Int, Long], k: Int, vOld: Long, vNew: Long) =>
    import std.tuple._, std.anyVal._, std.map._

    val mWithout = m0 - k
    val mWithOld = m0 + (k -> vOld)
    val mWithNew = m0 + (k -> vNew)

    // not already in map
    getOrAdd[Id.Id, Int, Long](mWithout, k)(vNew) must_=== (mWithNew -> vNew)

    // already in map
    getOrAdd[Id.Id, Int, Long](mWithOld, k)(vNew) must_=== (mWithOld -> vOld)

    // lazy
    var evaluated = false
    getOrAdd[Id.Id, Int, Long](mWithOld, k)({evaluated = true; vNew}) must_=== (mWithOld -> vOld)
    evaluated must_=== false
  }
}
