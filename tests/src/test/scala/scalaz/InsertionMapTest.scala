package scalaz

import scalaz.scalacheck.ScalazArbitrary._

class InsertionMapTest extends Spec {

  "isEmpty == keys.isEmpty" ! check {
    (a: InsertionMap[Int, String]) => a.isEmpty == a.keys.isEmpty
  }

  "isEmpty == (size == 0)" ! check {
    (a: InsertionMap[Int, String]) => a.isEmpty == (a.size == 0)
  }

  "insert contains" ! check {
    (k: Int, v: String, a: InsertionMap[Int, String]) => a ^+^ (k, v) contains k
  }

  "insert get" ! check {
    (k: Int, v: String, a: InsertionMap[Int, String]) =>
      (a ^+^ (k, v) get k) == Some(v)
  }

  "insert in order" ! check {
    (k: Int, v: String, a: InsertionMap[Int, String]) =>
      (a ^+^ (k, v)).keys.head == k
  }

  "double insert in order" ! check {
    (k1: Int, v1: String, k2: Int, v2: String, a: InsertionMap[Int, String]) => {
      val q = (a ^+^ (k1, v1) ^+^ (k2, v2)).toList
      if (k1 == k2)
        q.head == (k2, v2)
      else
        q match {
          case r::s::_ => r == (k2, v2) && s == (k1, v1)
          case _ => false
        }
    }
  }

  "insert remove" ! check {
    (k: Int, v: String, a: InsertionMap[Int, String]) =>
      (a ^+^ (k, v) ^-^ k).toList == (a ^-^ k).toList
  }

  "remove not get" ! check {
    (k: Int, a: InsertionMap[Int, String]) =>
      ((a ^-^ k) get k) == None
  }

  "get getOr" ! check {
    (k: Int, z: String, a: InsertionMap[Int, String]) =>
      (a getOr (k, z)) == (a get k getOrElse z)
  }

  "remove pair get" ! check {
    (k: Int, a: InsertionMap[Int, String]) => {
      val (q, _) = a @- k
      q == (a get k)
    }
  }

  "remove pair removes" ! check {
    (k: Int, a: InsertionMap[Int, String]) => {
      val (_, r) = a @- k
      r.toList == (a ^-^ k).toList
    }
  }

}
