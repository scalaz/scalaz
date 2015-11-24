package scalaz

import org.scalacheck.Prop.forAll
import scala.util.Random

object MapTest extends SpecLite {
  import org.scalacheck.Arbitrary
  import scalaz.scalacheck.ScalazProperties._
  import scalaz.scalacheck.ScalazArbitrary._
  import std.anyVal._
  import std.list._
  import std.string._
  import std.option._
  import std.tuple._
  import syntax.std.option._

  import ==>>._

  def structurallySound[A: Order: Show, B: Equal: Show](m: A ==>> B) = {
    val al = m.toAscList
    al must_===(al.sortBy(_._1)(Order[A].toScalaOrdering))
  }

  "findLeft/findRight" in {
    val a = ==>>.fromFoldable((1 to 5).map(n => n -> n).toList)
    Foldable[Int ==>> ?].findLeft(a)(_ % 2 == 0) must_=== Some(2)
    Foldable[Int ==>> ?].findRight(a)(_ % 2 == 0) must_=== Some(4)
  }

  "findLeft" ! forAll{ (a: Int ==>> Int) =>
    val f = (_: Int) % 3 == 0
    Foldable[Int ==>> ?].findLeft(a)(f) must_=== Foldable[List].findLeft(a.values)(f)
  }

  "findRight" ! forAll{ (a: Int ==>> Int) =>
    val f = (_: Int) % 3 == 0
    Foldable[Int ==>> ?].findRight(a)(f) must_=== Foldable[List].findRight(a.values)(f)
  }

  "index" ! forAll { (a: Int ==>> Int, i: Byte) =>
    val F = Foldable[Int ==>> ?]
    F.index(a, i) must_=== a.toList.lift(i).map(_._2)
    F.index(a, -1) must_=== None
    F.index(a, 0) must_=== a.findMin.map(_._2)
    F.index(a, a.size - 1) must_=== a.findMax.map(_._2)
    F.index(a, a.size) must_=== None
  }

  "equals/hashCode" ! forAll { a: Int ==>> Int =>
    val b = ==>>.fromList(Random.shuffle(a.toList))
    a must_== b
    a.## must_=== b.##
  }

  "minViewWithKey" ! forAll { a: Int ==>> Int =>
    a.minViewWithKey match {
      case None =>
        a.size must_=== 0
      case Some(b) =>
        structurallySound(b._2)
        a.findMin must_=== Some(b._1)
        (b._2.size + 1) must_=== a.size
        (b._2 + b._1) must_=== a
    }
  }

  "maxViewWithKey" ! forAll { a: Int ==>> Int =>
    a.maxViewWithKey match {
      case None =>
        a.size must_=== 0
      case Some(b) =>
        structurallySound(b._2)
        a.findMax must_=== Some(b._1)
        (b._2.size + 1) must_=== a.size
        (b._2 + b._1) must_=== a
    }
  }

  "findMin" ! forAll { a: Int ==>> Int =>
    a.findMin must_=== {
      if(a.isEmpty) None
      else Some(a.toList.minBy(_._1))
    }
  }

  "findMax" ! forAll { a: Int ==>> Int =>
    a.findMax must_=== {
      if(a.isEmpty) None
      else Some(a.toList.maxBy(_._1))
    }
  }

  "deleteMin" ! forAll { a: Int ==>> Int =>
    val b = a.deleteMin
    structurallySound(b)
    if(a.isEmpty){
      b.isEmpty must_=== true
    }else{
      (b.size + 1) must_=== a.size
      b must_=== a.delete(a.findMin.get._1)
    }
  }

  "deleteMax" ! forAll { a: Int ==>> Int =>
    val b = a.deleteMax
    structurallySound(b)
    if(a.isEmpty){
      b.isEmpty must_=== true
    }else{
      (b.size + 1) must_=== a.size
      b must_=== a.delete(a.findMax.get._1)
    }
  }

  "deleteAt" ! forAll { (a: Int ==>> Int, i: Byte) =>
    if(a.size != 0){
      val n = i.toInt.abs % a.size
      val b = a.deleteAt(n)
      structurallySound(b)
      (a.size - 1) must_=== b.size
      b.member(a.keys(n)) must_=== false
      (b + a.elemAt(n).get) must_=== a
    }
  }

  "==>> fromList" ! {
    fromList(List.empty[(Int, String)]) must_===(empty[Int, String])
    fromList(List(5 -> "a", 3 -> "b")) must_===(fromList(List(3 -> "b", 5 -> "a")))
    fromList(List(5 ->"a", 3 -> "b", 5 -> "c")) must_===(fromList(List(5 -> "c", 3 -> "b")))
    fromList(List(5 -> "c", 3 -> "b", 5 -> "a")) must_===(fromList(List(5 -> "a", 3 -> "b")))
  }

  "Testing empty map" should {
    "be 0 for an empty map" in {
      empty.size must_== 0
    }
    "be 1 for a singleton map" in {
      singleton(1, 'a').size must_== 1
    }
    "be 3 for the list [(1,'a'), (2,'c'), (3,'b')])" in {
      fromList(List((1,'a'), (2,'c'), (3,'b'))).size must_== 3
    }
  }

  "Membership in map" should {
    "be true if element contained" in {
      fromList(List((5, 'a'), (3, 'b'))).member(5) must_== true
    }
    "be false if not contained" in {
      fromList(List((5, 'a'), (3, 'b'))).member(1) must_== false
    }
  }

  "Non-membership" should {
    "be false for contained member" in {
      fromList(List((5, 'a'), (3, 'b'))).notMember(5) must_== false
    }
    "be true for non-contained member" in {
      fromList(List((5, 'a'), (3, 'b'))).notMember(1) must_== true
    }
  }

  "elemAt" should {
    val d = fromList(List(5 -> "a", 3 -> "b"))
    "find successful result" in {
      d.elemAt(0) must_== (3, "b").some
      d.elemAt(1) must_== (5, "a").some
    }

    "not find a match" in {
      d.elemAt(2) must_== None
    }

    "elemAt" ! forAll { (a: Byte ==>> Int, b: Byte) =>
      a.elemAt(b) must_=== a.toList.lift(b)
    }
  }

  "==>> conversions" should {
    "toAscList" in {
      import std.tuple._
      implicit val listTupleShow = Show[List[(Int, String)]]

      fromList(List(5 -> "a", 3 -> "b")).toAscList must_===(fromList(List(3 -> "b", 5 -> "a")).toAscList)
      fromList(List(5 -> "a", 7 -> "c", 3 -> "b")) must_===(fromList(List(3 -> "b", 5 -> "a", 7 -> "c")))
    }
  }

  "==>> lookups" should {
    val d = fromList(List(5 -> "a", 3 -> "b"))

    "value lookup" in {
      fromList(List(("John","Sales"), ("Bob","IT"))).lookup("John") must_===(Some("Sales"))
      fromList(List(("John","Sales"), ("Bob","IT"))).lookup("Sarah") must_===(None)
    }

    "index lookup" in {
      d.lookupIndex(2).isDefined must_== false
      d.lookupIndex(3).get must_== 0
      d.lookupIndex(5).get must_== 1
      d.lookupIndex(6).isDefined must_== false
    }

    "lookup" ! forAll { (a: Byte ==>> Int, n: Byte) =>
      a.lookup(n) must_=== a.toList.find(_._1 == n).map(_._2)
    }

    "lookupAssoc" ! forAll { (a: Byte ==>> Int, n: Byte) =>
      a.lookupAssoc(n) must_=== a.lookup(n).map(n -> _)
    }

    "lookupIndex" ! forAll { (a: Byte ==>> Int, n: Byte) =>
      val x = a.keys.indexOf(n)
      a.lookupIndex(n) must_=== (if(x < 0) None else Some(x))
      a.lookupIndex(n).foreach{ b =>
        a.elemAt(b).map(_._1) must_=== Some(n)
      }
    }
  }

  "updateAt" should {
    "succeed" in {
      // TODO: This is a problem as the function 'updateAt' is not total - any thoughts?

      //-- > updateAt (\ _ _ -> Just "x") 0    (fromList [(5,"a"), (3,"b")]) == fromList [(3, "x"), (5, "a")]
      //-- > updateAt (\ _ _ -> Just "x") 1    (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "x")]
      //-- > updateAt (\ _ _ -> Just "x") 2    (fromList [(5,"a"), (3,"b")])    Error: index out of range
      //-- > updateAt (\ _ _ -> Just "x") (-1) (fromList [(5,"a"), (3,"b")])    Error: index out of range
      //-- > updateAt (\_ _  -> Nothing)  0    (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
      //-- > updateAt (\_ _  -> Nothing)  1    (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
      //-- > updateAt (\_ _  -> Nothing)  2    (fromList [(5,"a"), (3,"b")])    Error: index out of range
      //-- > updateAt (\_ _  -> Nothing)  (-1) (fromList [(5,"a"), (3,"b")])    Error: index out of range
      val d = fromList(List(5 -> "a", 3 -> "b"))

      d.updateAt(0, (_, _) => "x".some) must_===(fromList(List(3 -> "x", 5 -> "a")))
      d.updateAt(1, (_, _) => "x".some) must_===(fromList(List(3 -> "b", 5 -> "x")))
      //d.updateAt(2, (_, _) => "x".some) must_===(empty[Int, String])

      d.updateAt(0, (_, _) => None) must_===(singleton(5, "a"))
      d.updateAt(1, (_, _) => None) must_===(singleton(3, "b"))
    }
  }

  "delete" should {
    "remove an element" in {
      fromList(List((5,"a"), (3,"b"))).delete(5) must_===(singleton(3, "b"))
    }
    "not remove an element" in {
      fromList(List((5,"a"), (3,"b"))).delete(7) must_===(fromList(List((5,"a"), (3,"b"))))
    }
    "not remove from an empty map" in {
      empty.delete(5) must_===(empty[Int, Int])
    }
    "be sound" ! forAll {(m: Int ==>> Int, i: Int) =>
      val a = m delete i
      structurallySound(a)
      (a member i) must_=== false
      if(m member i)
        (m.size - 1) must_=== a.size
      else
        m must_=== a
    }
  }

  "==>> insertion" should {
    "insert" in {
      fromList(List(5 -> "a", 3 -> "b")).insert(5, "x") must_===(fromList(List(3 -> "b", 5 -> "x"))) // Replacement
      fromList(List((5,"a"), (3,"b"))).insert(7,"x") must_===(fromList(List((3,"b"), (5,"a"), (7,"x")))) // Addition of new key
      empty.insert(5, "x") must_===(singleton(5, "x"))
    }

    "insert sound" ! forAll {(m: Int ==>> Int, a: Int, b: Int) =>
      val c = m insert (a, b)
      structurallySound(c)
      if(m member a)
        m.size must_=== c.size
      else
        (m.size + 1) must_=== c.size
      c.lookup(a) must_=== Some(b)
    }

    "insertWith" in {
      val r = fromList(List(5 -> "a", 3 -> "b")).insertWith(_ + _, 5, "xxx")
      r must_===(fromList(List(3 -> "b", 5 -> "xxxa")))

      fromList(List(5 -> "a", 3 -> "b")).insertWith(_ + _, 7, "xxx") must_===(fromList(List(3 -> "b", 5 -> "a", 7 -> "xxx")))
      empty[Int, String].insertWith(_ + _, 5, "xxx") must_===(singleton(5, "xxx"))
    }

    "insertWithKey" in {
      val f = (k: Int, a: String, b: String) => k.toString + ":" + a + "|" + b
      fromList(List(5 -> "a", 3 -> "b")).insertWithKey(f, 5, "xxx") must_===(fromList(List(3 -> "b", 5 -> "5:xxx|a")))
      fromList(List(5 -> "a", 3 -> "b")).insertWithKey(f, 7, "xxx") must_===(fromList(List(3 -> "b", 5 -> "a", 7 -> "xxx")))
      empty.insertWithKey(f, 5, "xxx") must_===(singleton(5, "xxx"))
    }

    "insertWithKey sound" ! forAll {(m: Int ==>> Int, a: Int, b: Int) =>
      structurallySound(m insertWithKey ((_, _, b) => b, a, b))
    }
  }

  "==>> from a list" should {
    "equivalence to insert on empty ==>>" in {
      empty.insert(2,3).insert(3,4) must_===(fromList(List(2 -> 3, 3 -> 4)))
    }

    "be order-insensitive" ! forAll {(am: Map[Int, Int]) =>
      val al = am.toList
      fromList(al.reverse) must_===(fromList(al))
    }

    "create a valid map from empty list" in {
      fromList(List.empty[(Int, Int)]) must_===(empty[Int, Int])
    }
  }

  "==>> union operations" should {
    "be sound" ! forAll {(a: Int ==>> Int, b: Int ==>> Int) =>
      val c = a union b
      structurallySound(c)
      (a.keySet union b.keySet) must_=== c.keySet
    }

    "union" in {
      fromList(List((5, "a"), (3, "b"))) union fromList(List((5, "A"), (7, "C"))) must_== fromList(List((3, "b"), (5, "a"), (7, "C")))
    }

    "commute" ! forAll {(a: Int ==>> Int, b: Int ==>> Int) =>
      (a unionWith b)(_ + _) must_===((b unionWith a)(_ + _))
    }

    "be idempotent" ! forAll {(a: Int ==>> Int, b: Int ==>> Int) =>
      val ab = a union b
      ab union a must_===(ab)
      ab union b must_===(ab)
    }

    "unions" in {
      unions(List(
        fromList(List((5, "a"), (3, "b"))),
        fromList(List((5, "A"), (7, "C"))),
        fromList(List((5, "A3"), (3, "B3")))
      )) must_== fromList(List((3, "b"), (5, "a"), (7, "C")))

      unions(List(fromList(List(5 -> "A3", 3 -> "B3")), fromList(List(5 -> "A", 7 -> "C")), fromList(List(5 -> "a", 3 -> "b")))) must_== fromList(List(3 -> "B3", 5 -> "A3", 7 -> "C"))
    }

    "unionWith" in {
      val adder = (a: String, b: String) => Semigroup[String].append(a, b)
      val r = fromList(List(5 -> "a", 3 -> "b")).unionWith(fromList(List(5 -> "A", 7 -> "C")))(adder)

      r must_== fromList(List(3 -> "b", 5 -> "aA", 7 -> "C"))
    }

    "unionWithKey" in {
      val f = (key: Int, left: String, right: String) => key.toString + ":" + left + "|" + right
      val r = fromList(List(5 -> "a", 3 -> "b")).unionWithKey(fromList(List(5 -> "A", 7 -> "C")))(f)

      r must_== fromList(List(3 -> "b", 5 -> "5:a|A", 7 -> "C"))
    }
  }

  "==>> difference operations" should {
    "difference" in {
      val r = fromList(List(5 -> "a", 3 -> "b")).difference(fromList(List(5 -> "A", 7 -> "C")))
      r must_== singleton[Int, String](3, "b")
    }

    "be idempotent" ! forAll {(a: Int ==>> Int, b: Int ==>> Int) =>
      val ab = a \\ b
      (ab \\ b) must_===(ab)
    }

    "be idempotent (in one case)" in {
      val a = Bin(-1768028150,1831400640,Bin(-2147483648,2147483647,Tip(),Tip()),
                  Bin(-541865171,1,Tip(),Bin(1085869916,1066820187,Tip(),Tip())))
      val b = Bin(0,1979991171,Tip(),Tip())
      val ab = a \\ b
      (ab \\ b) must_===(ab)
    }

    "produce right keyset" ! forAll {(a: Int ==>> Int, b: Int ==>> Int) =>
      (a \\ b).keySet must_== (a.keySet \\ b.keySet)
    }

    "be an inverse" ! forAll {(a: Int ==>> Int) =>
      (a \\ a) must_===(==>>.empty[Int, Int])
    }

    "syntax" in {
      val r = fromList(List(5 -> "a", 3 -> "b")) \\ fromList(List(5 -> "A", 7 -> "C"))
      r must_== singleton[Int, String](3, "b")
    }

    "differenceWith" in {
      val f = (al: String, ar: String) => if (al == "b") Some(al + ":" + ar) else None
      fromList(List(5 -> "a", 3 -> "b")).differenceWith(fromList(List(5 -> "A", 3 -> "B", 7 -> "C")), f) must_===(singleton(3, "b:B"))
    }

    "differenceWithKey" in {
      val f = (k: Int, al: String, ar: String) => if (al == "b") Some(k.toString + ":" + al + "|" + ar) else None
      fromList(List(5 -> "a", 3 -> "b")).differenceWithKey(fromList(List(5 -> "A", 3 -> "B", 10 -> "C")), f) must_===(singleton(3, "3:b|B"))
    }
  }

  "==>> intersection operations" should {
    "intersection" in {
      val r = fromList(List(5 -> "a", 3 -> "b")) intersection fromList(List(5 -> "A", 7 -> "C"))
      r must_== singleton(5, "a")
    }

    "intersect soundly" ! forAll {(a: Int ==>> Int, b: Int ==>> Int) =>
      structurallySound(a intersection b)
    }

    "form an identity" ! forAll {(a: Int ==>> Int) =>
      a intersection a must_===(a)
    }

    "commute" ! forAll {(a: Int ==>> Int, b: Int ==>> Int) =>
      (a intersectionWith b)(_ + _) must_===((b intersectionWith a)(_ + _))
    }

    "commute (in one case)" in {
      val a = Bin(1951314151,1,Bin(-1,1271148582,Tip(),Tip()),
                  Bin(2147483647,-1423766788,Tip(),Tip()))
      val b = Bin(-12693552,-2147483648,
                  Bin(-1587083834,-729342404,Tip(),Tip()),
                  Bin(-1,0,Tip(),Tip()))
      (a intersectionWith b)(_ + _) must_===((b intersectionWith a)(_ + _))
    }

    "intersectionWith" in {
      val f = (a: String, b: String) => a + b
      val r = fromList(List(5 -> "a", 3 -> "b")).intersectionWith(fromList(List(5 -> "A", 7 -> "C")))(f)
      r must_== singleton(5, "aA")
    }

    "intersectionWithKey" in {
      val f = (k: Int, al: String, ar: String) => k.toString + ":" + al + "|" + ar
      val r = fromList(List(5 -> "a", 3 -> "b")).intersectionWithKey(fromList(List(5 -> "A", 7 -> "C")))(f)
      r must_== singleton(5, "5:a|A")
    }
  }

  "==>> update" should {
    "adjust" in {
      val f = "new " + (_: String)

      fromList(List(5 -> "a", 3 -> "b")).adjust(5, f) must_===(fromList(List(3 -> "b", 5 -> "new a")))
      fromList(List(5 -> "a", 3 -> "b")).adjust(7, f) must_===(fromList(List(3 -> "b", 5 -> "a")))
      empty[Int, String].adjust(7, f) must_===(empty[Int, String])
    }

    "adjustWithKey" in {
      val f = (k: Int, x: String) => k.toString + ":new " + x

      fromList(List(5 -> "a", 3 -> "b")).adjustWithKey(5, f) must_===(fromList(List(3 -> "b", 5 -> "5:new a")))
      fromList(List(5 -> "a", 3 -> "b")).adjustWithKey(7, f) must_===(fromList(List(3 -> "b", 5 -> "a")))
      empty[Int, String].adjustWithKey(7, f) must_===(empty[Int, String])
    }

    "update" in {
      val f = (x: String) => if (x == "a") Some("new a") else None

      fromList(List(5 -> "a", 3 -> "b")).update(5, f) must_===(fromList(List(3 -> "b", 5 -> "new a")))
      fromList(List(5 -> "a", 3 -> "b")).update(7, f) must_===(fromList(List(3 -> "b", 5 -> "a")))
      fromList(List(5 -> "a", 3 -> "b")).update(3, f) must_===(singleton(5, "a"))
    }

    "updateWithKey" in {
      val f = (k: Int, x: String) => if (x == "a") Some(k.toString + ":new a") else None

      fromList(List(5 -> "a", 3 -> "b")).updateWithKey(5, f) must_===(fromList(List(3 -> "b", 5 -> "5:new a")))
      fromList(List(5 -> "a", 3 -> "b")).updateWithKey(7, f) must_===(fromList(List(3 -> "b", 5 -> "a")))
      fromList(List(5 -> "a", 3 -> "b")).updateWithKey(3, f) must_===(singleton(5, "a"))
    }

    "updateLookupWithKey" in {
      import std.tuple._
      val f = (k: Int, x: String) => if (x == "a") Some(k.toString + ":new a") else None

      fromList(List(5 -> "a", 3 -> "b")).updateLookupWithKey(5, f) must_===((Some("5:new a"), fromList(List(3 -> "b", 5 -> "5:new a"))))
      fromList(List(5 -> "a", 3 -> "b")).updateLookupWithKey(7, f) must_===((None, fromList(List(3 -> "b", 5 -> "a"))))
      fromList(List(5 -> "a", 3 -> "b")).updateLookupWithKey(3, f) must_===((Some("b"), singleton(5, "a")))
    }

    "alter" in {
      val f1 = (_: Option[String]) => none[String]
      fromList(List(5 -> "a", 3 -> "b")).alter(7, f1) must_===(fromList(List(3 -> "b", 5 -> "a")))
      fromList(List(5 -> "a", 3 -> "b")).alter(5, f1) must_===(singleton(3, "b"))

      val f2 = (_: Option[String]) => "c".some
      fromList(List(5 -> "a", 3 -> "b")).alter(7, f2) must_===(fromList(List(3 -> "b", 5 -> "a", 7 -> "c")))
      fromList(List(5 -> "a", 3 -> "b")).alter(5, f2) must_===(fromList(List(3 -> "b", 5 -> "c")))
    }
  }

  "==>> submap" should {
    "isSubmapOfBy -> true" in {
      val o = implicitly[Order[Int]]
      fromList(List('a' -> 1)).isSubmapOfBy(fromList(List('a' -> 1, 'b' -> 2)), o.equal) must_== true
      fromList(List('a' -> 1)).isSubmapOfBy(fromList(List('a' -> 1, 'b' -> 2)), o.lessThanOrEqual) must_== true
      fromList(List('a' -> 1, 'b' -> 2)).isSubmapOfBy(fromList(List('a' -> 1, 'b' -> 2)), o.equal) must_== true
    }

    "isSubmapOfBy -> false" in {
      val o = implicitly[Order[Int]]
      fromList(List('a' -> 2)).isSubmapOfBy(fromList(List('a' -> 1, 'b' -> 2)), o.equal) must_== false
      fromList(List('a' -> 1)).isSubmapOfBy(fromList(List('a' -> 1, 'b' -> 2)), o.lessThan) must_== false
      fromList(List('a' -> 1, 'b' -> 2)).isSubmapOfBy(fromList(List('a' -> 1)), o.equal) must_== false
    }

    "isSubmapOf" ! forAll { (a: Byte ==>> Byte, b: Byte ==>> Byte) =>
      if(a isSubmapOf b){
        (a.keySet isSubsetOf b.keySet) must_=== true
        a.difference(b) must_=== ==>>.empty
        a.toList.foreach{case (k, v) => b.lookup(k) must_=== Some(v)}
      }
    }
  }

  "==>> filter" should {
    val m = fromList(List(5 -> "a", 3 -> "b"))

    "filter" in {
      m.filter(_ > "a") must_===(singleton(3, "b"))
      m.filter(_ > "x") must_===(empty[Int, String])
      m.filter(_ < "a") must_===(empty[Int, String])
    }

    "be sound" ! forAll { (a: Byte ==>> Byte, n: Byte) =>
      val b = a.filter(_ > n)
      structurallySound(b)
      fromList(a.toList.filter(_._2 > n)) must_=== b
    }
  }

  "==>> partition" should {
    val m = fromList(List(5 -> "a", 3 -> "b"))

    "partition" in {
      m.partition(_ > "a") must_===((singleton(3, "b"), singleton(5, "a")))
      m.partition(_ < "x") must_===((fromList(List(3 -> "b", 5 -> "a")), empty[Int, String]))
      m.partition(_ > "x") must_===((empty[Int, String], fromList(List(3 -> "b", 5 -> "a"))))
    }

    "be sound" ! forAll { (m: Int ==>> Int, n: Int) =>
      val (ma, mb) = m.partition(n > _)
      structurallySound(ma)
      structurallySound(mb)
      (ma union mb) must_=== m
    }

    "partitionWithKey" in {
      m.partitionWithKey((k, _) => k > 3) must_===((singleton(5, "a"), singleton(3, "b")))

      m.partitionWithKey((k, _) => k < 7) must_===((fromList(List(3 -> "b", 5 -> "a")), empty[Int, String]))
      m.partitionWithKey((k, _) => k > 7) must_===((empty[Int, String], fromList(List(3 -> "b", 5 -> "a"))))
    }
  }

  "==>> map" should {
    import std.tuple._

    "map" in {
      fromList(List(5 -> "a", 3 -> "b")).map(_ + "x") must_===(fromList(List(3 -> "bx", 5 -> "ax")))
    }

    "mapWithKey" in {
      val f = (k: Int, x: String) => k.toString + ":" + x
      fromList(List(5 -> "a", 3 -> "b")).mapWithKey(f) must_===(fromList(List(3 -> "3:b", 5 -> "5:a")))
    }

    "mapAccum" in {
      val f = (a: String, b: String) => (a + b, b + "X")
      fromList(List(5 -> "a", 3 -> "b")).mapAccum("Everything: ")(f) must_===("Everything: ba", fromList(List(3 -> "bX", 5 -> "aX")))
    }

    "mapAccumWithKey" in {
      val f = (a: String, k: Int, b: String) => (a + " " + k.toString + "-" + b, b + "X")
      fromList(List(5 -> "a", 3 -> "b")).mapAccumWithKey("Everything:")(f) must_===("Everything: 3-b 5-a", fromList(List(3 -> "bX", 5 -> "aX")))
    }

    "mapKeys" in {
      fromList(List(5 -> "a", 3 -> "b")).mapKeys(_ + 1) must_===(fromList(List(4 -> "b", 6 -> "a")))
      fromList(List(1 -> "b", 2 -> "a", 3 -> "d", 4 -> "c")).mapKeys(_ => 1) must_===(singleton(1, "c"))
      fromList(List(1 -> "b", 2 -> "a", 3 -> "d", 4 -> "c")).mapKeys(_ => 3) must_===(singleton(3, "c"))
    }

    "mapKeys sound" ! forAll { a: Int ==>> Int =>
      val b = a.mapKeys(identity)
      b must_=== a
      structurallySound(b)
      val f = (_: Int) % 10
      val c = a.mapKeys(f)
      c must_=== fromList(a.toList.map(x => (f(x._1), x._2)))
      structurallySound(c)
    }

    "mapWithKeys" in {
      fromList(List(1 -> "b", 2 -> "a", 3 -> "d", 4 -> "c")).mapKeysWith(_ => 1, _ + _) must_===(singleton(1, "cdab"))
      fromList(List(1 -> "b", 2 -> "a", 3 -> "d", 4 -> "c")).mapKeysWith(_ => 3, _ + _) must_===(singleton(3, "cdab"))
    }

    "mapOption" in {
      val f = (x: String) => if (x == "a") Some("new a") else None
      fromList(List(5 -> "a", 3 -> "b")).mapOption(f) must_===(singleton(5, "new a"))
    }

    "mapOptionWithKey" in {
      val f = (k: Int, _: String) => if (k < 5) Some("key : " + k.toString) else None
      fromList(List(5 -> "a", 3 -> "b")).mapOptionWithKey(f) must_===(singleton(3, "key : 3"))
    }

    "mapEither" in {
      val f = (a: String) => if (a < "c") \/.left(a) else \/.right(a)
      val lst = fromList(List(5 -> "a", 3 -> "b", 1 -> "x", 7 -> "z"))

      lst.mapEither(f) must_===((fromList(List(3 -> "b", 5 -> "a")), fromList(List(1 -> "x", 7 -> "z"))))
      lst.mapEither((a: String) => \/.right(a)) must_===((empty[Int, String], lst))
    }

    "mapEitherWithKey" in {
      val f = (k: Int, a: String) => if (k < 5) \/.left(k * 2) else \/.right(a + a)
      val lst = fromList(List(5 -> "a", 3 -> "b", 1 -> "x", 7 -> "z"))

      lst.mapEitherWithKey(f) must_===(fromList(List(1 -> 2, 3 -> 6)), fromList(List(5 -> "aa", 7 -> "zz")))
      lst.mapEitherWithKey((_: Int, a: String) => \/.right(a)) must_===(empty[Int, String], lst)
    }
  }

  "==>> fold" should {
    "fold" in {
      val f = (a: Int, b: String) => a + b.length
      fromList(List(5 -> "a", 3 -> "bbb")).fold(0)((_, x, z) => f(z, x)) must_== 4
    }

    "foldrWithKey" in {
      val f = (k: Int, a: String, result: String) => result + "(" + k.toString + ":" + a + ")"
      fromList(List(5 -> "a", 3 -> "b")).foldrWithKey("Map: ")(f) must_== "Map: (5:a)(3:b)"
    }
  }

  "==>> list operations" should {
    "values" in {
      fromList(List(5 -> "a", 3 -> "b")).values must_===(List("b", "a"))
    }

    "keys" in {
      fromList(List(5 -> "a", 3 -> "b")).keys must_===(List(3, 5))
    }

    "keySet" in {
      fromList(List(5 -> "a", 3 -> "b")).keySet must_===(ISet.fromList(List(3, 5)))
      empty[Int, String].keySet must_===(ISet.empty[Int])
    }

    "fromList" in {
      fromList(List.empty[(Int, String)]) must_===(empty[Int, String])
      fromList(List(5 -> "a", 3 -> "b", 5 -> "c")) must_===(fromList(List(5 -> "c", 3 -> "b")))
      fromList(List(5 -> "c", 3 -> "b", 5 -> "a")) must_===(fromList(List(5 -> "a", 3 -> "b")))
    }

    "fromListWith" in {
      fromListWith(List(5 -> "a", 5 -> "b", 3 -> "b", 3 -> "a", 5 -> "a"))(_ + _) must_===(fromList(List(3 -> "ab", 5 -> "aba")))
      fromListWith(List.empty[(Int, String)])(_ + _) must_===(empty[Int, String])
    }

    "fromListWithKey" in {
      val f = (k: Int, a1: String, a2: String) => k.toString + a1 + a2

      fromListWithKey(List(5 -> "a", 5 -> "b", 3 -> "b", 3 -> "a", 5 -> "a"))(f) must_===(fromList(List(3 -> "3ab", 5 -> "5a5ba")))
      fromListWithKey(List.empty[(Int, String)])(f) must_===(empty[Int, String])
    }

    "toList" in {
      import std.tuple._
      fromList(List(5 -> "a", 3 -> "b")).toList must_===(List(3 -> "b", 5 -> "a"))
      empty[Int, String].toList must_===(List.empty[(Int, String)])
    }
  }

  /*"==>> validity" should {
    "valid" in {
      fromList(List(3 -> "b", 5 -> "a")).isValid must_== true
      //-- > valid (fromAscList [(3,"b"), (5,"a")]) == True
      //-- > valid (fromAscList [(5,"a"), (3,"b")]) == False
    }
  }*/

  checkAll(order.laws[Int ==>> Int])
  checkAll(monoid.laws[Int ==>> Int])

  {
    implicit def equMapConj[A: Equal, B: Equal]: Equal[(A ==>> B) @@ Tags.Conjunction] =
      Tag.subst(implicitly)

    implicit def arbMapConj[A, B](implicit a: Arbitrary[A ==>> B]
                                ): Arbitrary[(A ==>> B) @@ Tags.Conjunction] =
      Tag.subst(a)

    checkAll("conjunction", semigroup.laws[(Int ==>> Int) @@ Tags.Conjunction])
  }

  "align" ! forAll { (a: Int ==>> String, b: Int ==>> Long) =>
    import \&/._
    val F = Align[Int ==>> ?]
    val x = F.align(a, b)
    val keysA = a.keySet
    val keysB = b.keySet

    x must_=== F.alignWith[String, Long, String \&/ Long](identity)(a, b)
    x.keySet must_=== (keysA union keysB)

    x.filter(_.isThis).keySet must_=== (keysA difference keysB)
    x.filter(_.isThat).keySet must_=== (keysB difference keysA)
    x.filter(_.isBoth).keySet must_=== (keysA intersection keysB)

    x.filter(_.isThis) must_=== a.filterWithKey((k, _) => ! keysB.member(k)).map(This(_))
    x.filter(_.isThat) must_=== b.filterWithKey((k, _) => ! keysA.member(k)).map(That(_))
    x.filter(_.isBoth) must_=== a.filterWithKey((k, _) => keysB.member(k)).mapWithKey((k, v) => Both(v, b.lookup(k).get))
  }

  type IntMap[A] = Int ==>> A
  checkAll(traverse.laws[IntMap])
  checkAll(bind.laws[IntMap])
  checkAll(align.laws[IntMap])
  checkAll(zip.laws[IntMap])
  checkAll(bifoldable.laws[==>>])
  checkAll(FoldableTests.anyAndAllLazy[IntMap])

  object instances {
    def bind[A: Order] = Bind[A ==>> ?]
    def traverse[A] = Traverse[A ==>> ?]

    // checking absence of ambiguity
    def functor[A: Order] = Functor[A ==>> ?]
  }
}
