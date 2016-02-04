package scalaz
package std

import std.AllInstances._
import org.scalacheck.Arbitrary, Arbitrary.arbitrary
import scalaz.scalacheck.ScalazProperties._
import scala.collection.immutable.{Map => SMap, MapLike}
import scala.math.{Ordering => SOrdering}
import org.scalacheck.Prop.forAll

abstract class XMapTest[Map[K, V] <: SMap[K, V] with MapLike[K, V, Map[K, V]], BKC[_]]
  (dict: MapSubInstances with MapSubFunctions{
     type XMap[A, B] = Map[A, B]
     type BuildKeyConstraint[A] = BKC[A]
   })(implicit BKCF: Contravariant[BKC], OI: BKC[Int], OS: BKC[String]) extends SpecLite {
  import dict._

  checkAll(traverse.laws[Map[Int, ?]])
  checkAll(FoldableTests.anyAndAllLazy[Map[Int, ?]])
  checkAll(isEmpty.laws[Map[Int, ?]])
  checkAll(bind.laws[Map[Int, ?]])
  checkAll(align.laws[Map[Int, ?]])
  checkAll(monoid.laws[Map[Int,String]])
  checkAll(order.laws[Map[Int,String]])
  checkAll(equal.laws[Map[Int,String]])

  checkAll("satisfy equals laws when not natural", equal.laws[Map[NotNatural, String]])

  implicit def mapArb[A: Arbitrary: BKC, B: Arbitrary]: Arbitrary[Map[A, B]] =
    Arbitrary(arbitrary[SMap[A, B]] map (m => fromSeq(m.toSeq:_*)))

  class NotNatural(val id: Int)
  implicit def NotNaturalArbitrary: Arbitrary[NotNatural] =
    Arbitrary(arbitrary[Int] map (new NotNatural(_)))

  implicit def NotNaturalOrder: Order[NotNatural] =
    Order.orderBy[NotNatural, Int](_.id)

  implicit def NotNaturalBKC: BKC[NotNatural] = BKCF.contramap(OI)(_.id)

  implicit def NotNaturalEqual: Equal[NotNatural] = new Equal[NotNatural] {
    def equal(a1: NotNatural, a2: NotNatural): Boolean = a1.id == a2.id
  }

  "map ordering" ! forAll {
    val O = implicitly[Order[Map[String,Int]]]
    val O2 = SOrdering.Iterable(implicitly[SOrdering[(String,Int)]])
    (kvs: List[(String,Int)], kvs2: List[(String,Int)]) => {
      val (m1, m2) = (fromSeq(kvs:_*), fromSeq(kvs2:_*))
      ((m1.size == kvs.size) && (m2.size == kvs2.size)) ==> {
        val l: Boolean = O.lessThan(m1, m2)
        val r: Boolean = (if (m1.size < m2.size) true
                          else if (m1.size > m2.size) false
                          else O2.lt(kvs.sortBy(_._1), kvs2.sortBy(_._1)))
        l == r
      }
    }
  }

  "align" ! forAll { (a: Map[Int, String], b: Map[Int, Long]) =>
    import std.set._, \&/._
    val F = Align[Map[Int, ?]]
    val x = F.align(a, b)
    val keysA = a.keySet
    val keysB = b.keySet

    x must_=== F.alignWith[String, Long, String \&/ Long](identity)(a, b)
    ==>>.fromList(x.toList) must_=== Align[Int ==>> ?].align(==>>.fromList(a.toList), ==>>.fromList(b.toList))
    x.keySet must_=== (keysA ++ keysB)

    x.filter(_._2.isThis).keySet must_=== (keysA -- keysB)
    x.filter(_._2.isThat).keySet must_=== (keysB -- keysA)
    x.filter(_._2.isBoth).keySet must_=== (keysA & keysB)

    x.filter(_._2.isThis) must_=== F.map(a.filter{case (k, _) => ! keysB(k)})(This(_))
    x.filter(_._2.isThat) must_=== F.map(b.filter{case (k, _) => ! keysA(k)})(That(_))
  }

  "getOrAdd" ! forAll { (m0: Map[Int, Long], k: Int, vOld: Long, vNew: Long) =>
    import std.tuple._, std.anyVal._, std.map._

    val mWithout = m0 - k
    val mWithOld = m0 + (k -> vOld)
    val mWithNew = m0 + (k -> vNew)

    // not already in map
    getOrAdd[Id.Id, Int, Long](mWithout, k)(vNew) must_=== (mWithNew, vNew)

    // already in map
    getOrAdd[Id.Id, Int, Long](mWithOld, k)(vNew) must_=== (mWithOld, vOld)

    // lazy
    var evaluated = false
    getOrAdd[Id.Id, Int, Long](mWithOld, k)({evaluated = true; vNew}) must_=== (mWithOld, vOld)
    evaluated must_=== false
  }
}

private object DIContravariant extends Contravariant[λ[α => DummyImplicit]] {
  def contramap[A, B](fa: DummyImplicit)(f: B => A) = fa
}

object MapTest extends XMapTest[SMap, λ[α => DummyImplicit]](std.map)(DIContravariant, implicitly, implicitly)
