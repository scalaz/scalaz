package scalaz

import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

object ISetTest extends SpecLite {
  import org.scalacheck.Arbitrary
  import scalaz.scalacheck.ScalaCheckBinding._
  import scalaz.scalacheck.ScalazProperties._
  import scalaz.scalacheck.ScalazArbitrary._
  import syntax.std.option._
  import std.anyVal._
  import std.list._
  import std.option._
  import std.tuple._

  import ISet._

  // Check some laws
  checkAll(equal.laws[ISet[Int]])
  checkAll(order.laws[ISet[Int]])
  checkAll(monoid.laws[ISet[Int]])
  checkAll(foldable.laws[ISet])

  def structurallySound[A: Order: Show](s: ISet[A]) = {
    val al = s.toAscList
    al must_===(al.sorted)(Order[A].toScalaOrdering)
  }

  "member" ! forAll {(a: ISet[Int], i: Int) =>
    a.member(i) must_=== a.toList.contains(i)
  }

  "sound delete" ! forAll {(a: ISet[Int], i: Int) =>
    structurallySound(a delete i)
  }

  "sound insert" ! forAll {(a: ISet[Int], i: Int) =>
    structurallySound(a insert i)
  }

  "sound union" ! forAll {(a: ISet[Int], b: ISet[Int]) =>
    structurallySound(a union b)
  }

  "union commute" ! forAll {(a: ISet[Int], b: ISet[Int]) =>
    (a union b) must_===(b union a)
  }

  "union idempotent" ! forAll {(a: ISet[Int], b: ISet[Int]) =>
    val ab = a union b
    ab union a must_===(ab)
    ab union b must_===(ab)
  }

  "sound intersection" ! forAll {(a: ISet[Int], b: ISet[Int]) =>
    structurallySound(a intersection b)
  }

  "intersection commute" ! forAll {(a: ISet[Int], b: ISet[Int]) =>
    (a intersection b) must_===(b intersection a)
  }

  """\\ is idempotent""" ! forAll {(a: ISet[Int], b: ISet[Int]) =>
    val ab = a \\ b
    (ab \\ b).toList must_===(ab.toList)
  }

  "difference is an inverse" ! forAll {(a: ISet[Int]) =>
    (a difference a) must_===(ISet.empty[Int])
  }

  "sound difference" ! forAll {(a: ISet[Int], b: ISet[Int]) =>
    structurallySound(a difference b)
  }

  "filter" ! forAll {(a: ISet[Int], p: Int => Boolean) =>
    (a filter p).toList must_=== a.toList.filter(p)
  }

  "sound partition" ! forAll {(a: ISet[Int], p: Int => Boolean) =>
    val (ma, mb) = a partition p
    structurallySound(ma)
    structurallySound(mb)
  }

  "partition" ! forAll {(a: ISet[Int], p: Int => Boolean) =>
    val part = a partition p
    (part._1.toList, part._2.toList) must_=== a.toList.partition(p)
  }

  "map" ! forAll {(a: ISet[Int], f: Int => Int) =>
    a.map(f).toList must_=== a.toList.map(f).distinct.sorted
  }

  "map" ! forAll {(a: List[Int => Boolean]) =>
    ISet.fromList(a.map(_(2)).distinct.sorted) must_=== MSet.fromList(a).map(_(2))
  }

  "filter" ! forAll {(s: ISet[Int], p: Int => Boolean) =>
    s.filter(p).toList must_=== s.toList.sorted.filter(p)
  }

  "findMin" ! forAll {(a: ISet[Int]) =>
    a.findMin must_=== a.toList.sorted.headOption
  }

  "findMax" ! forAll {(a: ISet[Int]) =>
    a.findMax must_=== a.toList.sortWith(_ > _).headOption
  }

  "deleteMin" ! forAll {(a: ISet[Int]) =>
    a.deleteMin must_=== fromList(a.toList.drop(1))
  }

  "deleteMax" ! forAll {(a: ISet[Int]) =>
    a.deleteMax must_=== fromList(a.toList.sortWith(_ > _).drop(1))
  }

  "minView" ! forAll {(a: ISet[Int]) =>
    val l = a.toList.sorted
    val target = if (l.isEmpty) none else (l.head, fromList(l.tail)).some
    a.minView must_=== target
  }

  "maxView" ! forAll {(a: ISet[Int]) =>
    val l = a.toList.sortWith(_ > _)
    val target = if (l.isEmpty) none else (l.head, fromList(l.tail)).some
    a.maxView must_=== target
  }

}
