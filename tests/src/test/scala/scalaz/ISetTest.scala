package scalaz

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import scala.util.Random

object ISetTest extends SpecLite {
  import scalaz.scalacheck.ScalaCheckBinding._
  import scalaz.scalacheck.ScalazProperties._
  import scalaz.scalacheck.ScalazArbitrary._
  import syntax.std.option._
  import std.anyVal._
  import std.list._
  import std.option._
  import std.tuple._
  import std.string._

  import ISet._

  // Check some laws
  checkAll(equal.laws[ISet[Int]])
  checkAll(order.laws[ISet[Int]])
  checkAll(monoid.laws[ISet[Int]])
  checkAll(band.laws[ISet[Int]])
  checkAll(foldable.laws[ISet])
  checkAll(FoldableTests.anyAndAllLazy[ISet])

  def structurallySound[A: Order: Show](s: ISet[A]) = {
    val al = s.toAscList
    al must_===(al.sorted)(Order[A].toScalaOrdering)
  }

  private[this] case class Foo(a: Int)
  private[this] object Foo {
    implicit val show: Show[Foo] =
      Show.shows[Foo](_.a.toString)
    implicit val order: Order[Foo] =
      Order[Int].contramap(_.a)
    implicit val arb: Arbitrary[Foo] =
      Functor[Arbitrary].map(implicitly[Arbitrary[Int]])(Foo(_))
  }

  "show" ! forAll { a: ISet[Foo] =>
    Show[ISet[Foo]].shows(a) must_=== a.toAscList.map(_.a).mkString("ISet(", ",", ")")
  }

  "findLeft/findRight" in {
    val a = ISet.fromList(List(1, 2, 3, 4, 5))
    Foldable[ISet].findLeft(a)(_ % 2 == 0) must_=== Some(2)
    Foldable[ISet].findRight(a)(_ % 2 == 0) must_=== Some(4)
  }

  "findLeft" ! forAll{ a: ISet[Int] =>
    val f = (_: Int) % 3 == 0
    Foldable[ISet].findLeft(a)(f) must_=== Foldable[List].findLeft(a.toList)(f)
  }

  "findRight" ! forAll { a: ISet[Int] =>
    val f = (_: Int) % 3 == 0
    Foldable[ISet].findRight(a)(f) must_=== Foldable[List].findRight(a.toList)(f)
  }

  "index" ! forAll { (a: ISet[Int], i: Byte) =>
    val F = Foldable[ISet]
    F.index(a, i) must_=== a.toList.lift(i)
    F.index(a, -1) must_=== None
    F.index(a, 0) must_=== a.findMin
    F.index(a, a.size - 1) must_=== a.findMax
    F.index(a, a.size) must_=== None
  }

  "toIList" ! forAll { a: ISet[Int] =>
    val b = Foldable[ISet].toIList(a)
    b.sorted must_=== b
  }

  "equals/hashCode" ! forAll { a: ISet[Int] =>
    val b = ISet.fromList(Random.shuffle(a.toList))
    a must_== b
    a.## must_=== b.##
  }

  "split" ! forAll { (a: ISet[Int], i: Int) =>
    val (b, c) = a.split(i)
    structurallySound(b)
    structurallySound(c)
    Foldable[ISet].all(b)(_ < i) must_=== true
    Foldable[ISet].all(c)(_ > i) must_=== true
    if(a member i){
      (b.size + c.size + 1) must_=== a.size
      b.union(c).insert(i) must_=== a
    }else{
      (b union c) must_=== a
    }
  }

  "splitMember" ! forAll { (a: ISet[Int], i: Int) =>
    val (b, c, d) = a.splitMember(i)
    structurallySound(b)
    structurallySound(d)
    c must_=== a.member(i)
    Foldable[ISet].all(b)(_ < i) must_=== true
    Foldable[ISet].all(d)(_ > i) must_=== true
    if(c){
      (b.size + d.size + 1) must_=== a.size
      b.union(d).insert(i) must_=== a
    }else{
      (b union d) must_=== a
    }
  }

  "splitRoot" ! forAll { a: ISet[Int] =>
    a match {
      case Tip() =>
        a.splitRoot must_=== List.empty[ISet[Int]]
      case s@Bin(_, _, _) =>
        val List(l, x, r) = s.splitRoot
        structurallySound(l)
        structurallySound(r)
        l must_=== s.l
        r must_=== s.r
        x must_=== singleton(s.a)
        l.union(r).union(x) must_=== s
    }
  }

  "lookupIndex" ! forAll { a: ISet[Int] =>
    val l = a.toList
    (0 until a.size) foreach { i =>
      a.lookupIndex(l(i)) must_=== Some(i)
    }
    (0 until 5) foreach { _ =>
      val r = Random.nextInt()
      if(a.member(r))
        a.lookupIndex(r) must_=== Some(l.indexOf(r))
      else
        a.lookupIndex(r) must_=== None
    }
  }

  "deleteAt" ! forAll { a: ISet[Int] =>
    val l = a.toList
    (0 until a.size) foreach { i =>
      val e = l(i)
      val b = a.deleteAt(i)
      structurallySound(b)
      b.notMember(e) must_=== true
      b.size must_=== (a.size - 1)
      b.insert(e) must_=== a
    }
    a.deleteAt(-1) must_=== a
    a.deleteAt(a.size) must_=== a
  }

  "lookupLT" ! forAll { (a: ISet[Int], i: Int) =>
    a.lookupLT(i) match {
      case Some(b) =>
        (i > b) must_=== true
        val (c, d) = a.split(i)
        c.findMax must_=== Option(b)
        Foldable[ISet].all(d)(_ > i) must_=== true
        a.filter(x => b < x && x < i) must_=== ISet.empty
      case None =>
        a.split(i)._1 must_=== ISet.empty
    }
  }

  "lookupGT" ! forAll { (a: ISet[Int], i: Int) =>
    a.lookupGT(i) match {
      case Some(b) =>
        (i < b) must_=== true
        a.split(i)._2.findMin must_=== Option(b)
      case None =>
        a.split(i)._2 must_=== ISet.empty
    }
  }

  "member" ! forAll {(a: ISet[Int], i: Int) =>
    a.member(i) must_=== a.toList.contains(i)
  }

  "sound delete" ! forAll {(a: ISet[Int], i: Int) =>
    val b = a.delete(i)
    structurallySound(b)
    if(a.member(i))
      (a.size - b.size) must_=== 1
    else
      a must_=== b
  }

  "sound insert" ! forAll {(a: ISet[Int], i: Int) =>
    val b = a.insert(i)
    structurallySound(b)
    if(a.member(i))
      a must_=== b
    else
      (b.size - a.size) must_=== 1
  }

  "sound union" ! forAll {(a: ISet[Int], b: ISet[Int]) =>
    (a union b) must_=== ISet.fromList(a.toList ++ b.toList)
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
    val c = a difference b
    structurallySound(c)
    Foldable[ISet].any(c)(b member _) must_=== false
    Foldable[ISet].all(c)(a member _) must_=== true
  }

  "filter" ! forAll {(a: ISet[Int], p: Int => Boolean) =>
    (a filter p).toList must_=== a.toList.filter(p)
  }

  "sound partition" ! forAll {(a: ISet[Int], p: Int => Boolean) =>
    val (ma, mb) = a partition p
    structurallySound(ma)
    structurallySound(mb)
    (ma.size + mb.size) must_=== a.size
    (ma union mb) must_=== a
  }

  "partition" ! forAll {(a: ISet[Int], p: Int => Boolean) =>
    val part = a partition p
    (part._1.toList, part._2.toList) must_=== a.toList.partition(p)
  }

  "map" ! forAll {(a: ISet[Int], f: Int => Int) =>
    a.map(f).toList must_=== a.toList.map(f).distinct.sorted
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

  "isSubsetOf" ! forAll { (a: ISet[Int], b: ISet[Int]) =>
    val c = a isSubsetOf b
    c must_=== (a.toList.toSet subsetOf b.toList.toSet)
    c must_=== Foldable[ISet].all(a)(b member _)
    (c && (b isSubsetOf a)) must_=== Equal[ISet[Int]].equal(a, b)
  }

}
