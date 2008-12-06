package fj.data

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import pre.{Equal, Hash}

object ArbitraryHashMap {
  implicit def arbitraryHashMap[K, V](implicit ak: Arbitrary[K], av: Arbitrary[V], e: Equal[K], h: Hash[K]): Arbitrary[HashMap[K, V]] =
    Arbitrary(arbitrary[scala.List[(K, V)]].map(kvs => {
      val m = new HashMap[K, V](e, h)
      kvs.foreach { case (k, v) => m.set(k, v) }
      m
    }))
}
