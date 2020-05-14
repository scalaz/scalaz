package scalaz

import org.scalacheck.Prop.forAll
import scalaz.Maybe.{Empty, Just, just}

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

  import ==>>._

  def structurallySound[A: Order: Show, B: Equal: Show](m: A ==>> B) = {
    isSortedByKey(m)
    isBalanced(m)
  }

  private def isSortedByKey[A: Order: Show, B: Equal: Show](m: A ==>> B) = {
    val al = m.toAscList
    al must_===(al.sortBy(_._1)(Order[A].toScalaOrdering))
  }

  private def isBalanced[A, B](m: A ==>> B) = {
    def isWeightsValid(l: A ==>> B, r: A ==>> B): Boolean = {
      val sizeL = if (l.size == 0) 1 else l.size
      val sizeR = if (r.size == 0) 1 else r.size

      (sizeL <= sizeR * delta) && (sizeR <= sizeL * delta)
    }

    def go(mab: A ==>> B): Boolean =
      mab match {
        case Tip() =>
          true
        case Bin(_, _, l, r) =>
          if (isWeightsValid(l, r)) go(l) && go(r)
          else false
      }

    go(m) must_=== true
  }

  "findLeft/findRight" in {
    val a = ==>>.fromFoldable((1 to 5).map(n => n -> n).toList)
    Foldable[Int ==>> *].findLeft(a)(_ % 2 == 0) must_=== Some(2)
    Foldable[Int ==>> *].findRight(a)(_ % 2 == 0) must_=== Some(4)
  }

  "findLeft" ! forAll{ (a: Int ==>> Int) =>
    val f = (_: Int) % 3 == 0
    Foldable[Int ==>> *].findLeft(a)(f) must_=== Foldable[IList].findLeft(a.values)(f)
  }

  "findRight" ! forAll{ (a: Int ==>> Int) =>
    val f = (_: Int) % 3 == 0
    Foldable[Int ==>> *].findRight(a)(f) must_=== Foldable[IList].findRight(a.values)(f)
  }

  "index" ! forAll { (a: Int ==>> Int, i: Byte) =>
    val F = Foldable[Int ==>> *]
    F.index(a, i) must_=== a.toList.lift(i).map(_._2)
    F.index(a, -1) must_=== None
    F.index(a, 0) must_=== a.findMin.map(_._2).toOption
    F.index(a, a.size - 1) must_=== a.findMax.map(_._2).toOption
    F.index(a, a.size) must_=== None
  }

  "equals/hashCode" ! forAll { (a: Int ==>> Int) =>
    val b = ==>>.fromList(Random.shuffle(a.toList))
    a must_== b
    a.## must_=== b.##
  }

  "minViewWithKey" ! forAll { (a: Int ==>> Int) =>
    a.minViewWithKey match {
      case Empty() =>
        a.size must_=== 0
      case Just(b) =>
        structurallySound(b._2)
        a.findMin must_=== just(b._1)
        (b._2.size + 1) must_=== a.size
        (b._2 + b._1) must_=== a
    }
  }

  "maxViewWithKey" ! forAll { (a: Int ==>> Int) =>
    a.maxViewWithKey match {
      case Empty() =>
        a.size must_=== 0
      case Just(b) =>
        structurallySound(b._2)
        a.findMax must_=== just(b._1)
        (b._2.size + 1) must_=== a.size
        (b._2 + b._1) must_=== a
    }
  }

  "findMin" ! forAll { (a: Int ==>> Int) =>
    a.findMin must_=== {
      if(a.isEmpty) Maybe.empty
      else just(a.toList.minBy(_._1))
    }
  }

  "findMax" ! forAll { (a: Int ==>> Int) =>
    a.findMax must_=== {
      if(a.isEmpty) Maybe.empty
      else just(a.toList.maxBy(_._1))
    }
  }

  "deleteMin" ! forAll { (a: Int ==>> Int) =>
    val b = a.deleteMin
    structurallySound(b)
    if(a.isEmpty){
      b.isEmpty must_=== true
    }else{
      (b.size + 1) must_=== a.size
      b must_=== a.delete(a.findMin.toOption.get._1)
    }
  }

  "deleteMax" ! forAll { (a: Int ==>> Int) =>
    val b = a.deleteMax
    structurallySound(b)
    if(a.isEmpty){
      b.isEmpty must_=== true
    }else{
      (b.size + 1) must_=== a.size
      b must_=== a.delete(a.findMax.toOption.get._1)
    }
  }

  "deleteAt" ! forAll { (a: Int ==>> Int, i: Byte) =>
    if(a.size != 0){
      val n = i.toInt.abs % a.size
      val b = a.deleteAt(n)
      structurallySound(b)
      (a.size - 1) must_=== b.size
      b.member(Foldable[IList].index(a.keys, n).get) must_=== false
      (b + a.elemAt(n).toOption.get) must_=== a
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
      d.elemAt(0) must_== just((3, "b"))
      d.elemAt(1) must_== just((5, "a"))
    }

    "not find a match" in {
      d.elemAt(2) must_== Maybe.empty
    }

    "elemAt" ! forAll { (a: Byte ==>> Int, b: Byte) =>
      a.elemAt(b) must_=== Maybe.fromOption(a.toList.lift(b))
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
      fromList(List(("John","Sales"), ("Bob","IT"))).lookup("John") must_=== just("Sales")
      fromList(List(("John","Sales"), ("Bob","IT"))).lookup("Sarah") must_=== Maybe.empty
    }

    "index lookup" in {
      d.lookupIndex(2).isJust must_== false
      d.lookupIndex(3) must_== just(0)
      d.lookupIndex(5) must_== just(1)
      d.lookupIndex(6).isJust must_== false
    }

    "value lookupLT" in {
      fromList(List((3,"a"), (5,"b"))).lookupLT(3) must_=== Maybe.empty
      fromList(List((3,"a"), (5,"b"))).lookupLT(4) must_=== just((3, "a"))
    }

    "value lookupGT" in {
      fromList(List((3,"a"), (5,"b"))).lookupGT(5) must_=== Maybe.empty
      fromList(List((3,"a"), (5,"b"))).lookupGT(3) must_=== just((5, "b"))
    }

    "value lookupLE" in {
      fromList(List((3,"a"), (5,"b"))).lookupLE(2) must_=== Maybe.empty
      fromList(List((3,"a"), (5,"b"))).lookupLE(3) must_=== just((3, "a"))
      fromList(List((3,"a"), (5,"b"))).lookupLE(4) must_=== just((3, "a"))
    }

    "value lookupGE" in {
      fromList(List((3,"a"), (5,"b"))).lookupGE(6) must_=== Maybe.empty
      fromList(List((3,"a"), (5,"b"))).lookupGE(5) must_=== just((5, "b"))
      fromList(List((3,"a"), (5,"b"))).lookupGE(4) must_=== just((5, "b"))
    }

    "lookup" ! forAll { (a: Byte ==>> Int, n: Byte) =>
      a.lookup(n).toOption must_=== a.toList.find(_._1 == n).map(_._2)
    }

    "lookupAssoc" ! forAll { (a: Byte ==>> Int, n: Byte) =>
      a.lookupAssoc(n) must_=== a.lookup(n).map(n -> _)
    }

    "lookupIndex" ! forAll { (a: Byte ==>> Int, n: Byte) =>
      val x = a.keys.indexOf(n)
      a.lookupIndex(n).toOption must_=== x
      a.lookupIndex(n).toOption.foreach { b =>
        a.elemAt(b).map(_._1) must_=== just(n)
      }
    }

    "lookupLT" ! forAll { (a: Int ==>> Int) =>
      if (a.size == 0) {
        val r = Random.nextInt()
        a.lookupLT(r) must_=== Maybe.empty
      }
      else {
        (0 until a.keys.length).foreach { i =>
          val (k, v) = a.elemAt(i).toOption.get

          a.lookupLT(k) must_=== a.elemAt(i-1)
          if (k != Int.MaxValue) {
            a.lookupLT(k+1) must_=== just((k, v))
          }
        }
      }
    }

    "lookupGT" ! forAll { (a: Int ==>> Int) =>
      if (a.size == 0) {
        val r = Random.nextInt()
        a.lookupGT(r) must_=== Maybe.empty
      }
      else {
        (0 until a.keys.length).foreach { i =>
          val (k, v) = a.elemAt(i).toOption.get

          a.lookupGT(k) must_=== a.elemAt(i+1)
          if (k != Int.MinValue) {
            a.lookupGT(k-1) must_=== just((k, v))
          }
        }
      }
    }

    "lookupLE" ! forAll { (a: Int ==>> Int) =>
      if (a.size == 0) {
        val r = Random.nextInt()
        a.lookupLE(r) must_=== Maybe.empty
      }
      else {
        (0 until a.keys.length).foreach { i =>
          val (k, v) = a.elemAt(i).toOption.get

          a.lookupLE(k) must_=== just((k, v))
          if (k != Int.MinValue) {
            a.lookupLE(k-1) must_=== a.elemAt(i-1)
          }
        }
      }
    }

    "lookupGE" ! forAll { (a: Int ==>> Int) =>
      if (a.size == 0) {
        val r = Random.nextInt()
        a.lookupGE(r) must_=== Maybe.empty
      }
      else {
        (0 until a.keys.length).foreach { i =>
          val (k, v) = a.elemAt(i).toOption.get

          a.lookupGE(k) must_=== just((k, v))
          if (k != Int.MaxValue) {
            a.lookupGE(k+1) must_=== a.elemAt(i+1)
          }
        }
      }
    }
  }

  "split" should {
    "splitRoot" ! forAll { (a: Int ==>> Int) =>
      a match {
        case Tip() =>
          a.splitRoot must_=== IList.empty[Int ==>> Int]
        case Bin(k, x, l, r) =>
          val ICons(l2, ICons(kv, ICons(r2, INil()))) = a.splitRoot
          structurallySound(l2)
          structurallySound(r2)
          l2 must_=== l
          r2 must_=== r
          kv must_=== singleton(k, x)
          l2.union(r2).union(kv) must_=== a
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

      d.updateAt(0, (_, _) => just("x")) must_=== fromList(List(3 -> "x", 5 -> "a"))
      d.updateAt(1, (_, _) => just("x")) must_=== fromList(List(3 -> "b", 5 -> "x"))
      //d.updateAt(2, (_, _) => "x".some) must_===(empty[Int, String])

      d.updateAt(0, (_, _) => Maybe.empty) must_=== singleton(5, "a")
      d.updateAt(1, (_, _) => Maybe.empty) must_=== singleton(3, "b")
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
      c.lookup(a) must_=== just(b)
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
      unions(IList(
        fromList(List((5, "a"), (3, "b"))),
        fromList(List((5, "A"), (7, "C"))),
        fromList(List((5, "A3"), (3, "B3")))
      )) must_== fromList(List((3, "b"), (5, "a"), (7, "C")))

      unions(IList(fromList(List(5 -> "A3", 3 -> "B3")), fromList(List(5 -> "A", 7 -> "C")), fromList(List(5 -> "a", 3 -> "b")))) must_== fromList(List(3 -> "B3", 5 -> "A3", 7 -> "C"))
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
      val f = (al: String, ar: String) => if (al == "b") just(al + ":" + ar) else Maybe.empty[String]
      fromList(List(5 -> "a", 3 -> "b")).differenceWith(fromList(List(5 -> "A", 3 -> "B", 7 -> "C")))(f) must_===(singleton(3, "b:B"))
    }

    "differenceWithKey" in {
      val f = (k: Int, al: String, ar: String) => if (al == "b") just(k.toString + ":" + al + "|" + ar) else Maybe.empty[String]
      fromList(List(5 -> "a", 3 -> "b")).differenceWithKey(fromList(List(5 -> "A", 3 -> "B", 10 -> "C")))(f) must_===(singleton(3, "3:b|B"))
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
      val f = (x: String) => if (x == "a") just("new a") else Maybe.empty[String]

      fromList(List(5 -> "a", 3 -> "b")).update(5, f) must_===(fromList(List(3 -> "b", 5 -> "new a")))
      fromList(List(5 -> "a", 3 -> "b")).update(7, f) must_===(fromList(List(3 -> "b", 5 -> "a")))
      fromList(List(5 -> "a", 3 -> "b")).update(3, f) must_===(singleton(5, "a"))
    }

    "updateWithKey" in {
      val f = (k: Int, x: String) => if (x == "a") just(k.toString + ":new a") else Maybe.empty[String]

      fromList(List(5 -> "a", 3 -> "b")).updateWithKey(5, f) must_===(fromList(List(3 -> "b", 5 -> "5:new a")))
      fromList(List(5 -> "a", 3 -> "b")).updateWithKey(7, f) must_===(fromList(List(3 -> "b", 5 -> "a")))
      fromList(List(5 -> "a", 3 -> "b")).updateWithKey(3, f) must_===(singleton(5, "a"))
    }

    "updateLookupWithKey" in {
      import std.tuple._
      val f = (k: Int, x: String) => if (x == "a") just(k.toString + ":new a") else Maybe.empty[String]

      fromList(List(5 -> "a", 3 -> "b")).updateLookupWithKey(5, f) must_===((just("5:new a"), fromList(List(3 -> "b", 5 -> "5:new a"))))
      fromList(List(5 -> "a", 3 -> "b")).updateLookupWithKey(7, f) must_===((Maybe.empty, fromList(List(3 -> "b", 5 -> "a"))))
      fromList(List(5 -> "a", 3 -> "b")).updateLookupWithKey(3, f) must_===((just("b"), singleton(5, "a")))
    }

    "alter" in {
      val f1 = (_: Maybe[String]) => Maybe.empty[String]
      fromList(List(5 -> "a", 3 -> "b")).alter(7, f1) must_===(fromList(List(3 -> "b", 5 -> "a")))
      fromList(List(5 -> "a", 3 -> "b")).alter(5, f1) must_===(singleton(3, "b"))

      val f2 = (_: Maybe[String]) => just("c")
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
        a.toList.foreach{case (k, v) => b.lookup(k) must_=== just(v)}
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
      fromList(List(5 -> "a", 3 -> "b")).mapAccum("Everything: ")(f) must_===("Everything: ba" -> fromList(List(3 -> "bX", 5 -> "aX")))
    }

    "mapAccumWithKey" in {
      val f = (a: String, k: Int, b: String) => (a + " " + k.toString + "-" + b, b + "X")
      fromList(List(5 -> "a", 3 -> "b")).mapAccumWithKey("Everything:")(f) must_===("Everything: 3-b 5-a" -> fromList(List(3 -> "bX", 5 -> "aX")))
    }

    "mapKeys" in {
      fromList(List(5 -> "a", 3 -> "b")).mapKeys(_ + 1) must_===(fromList(List(4 -> "b", 6 -> "a")))
      fromList(List(1 -> "b", 2 -> "a", 3 -> "d", 4 -> "c")).mapKeys(_ => 1) must_===(singleton(1, "c"))
      fromList(List(1 -> "b", 2 -> "a", 3 -> "d", 4 -> "c")).mapKeys(_ => 3) must_===(singleton(3, "c"))
    }

    "mapKeys sound" ! forAll { (a: Int ==>> Int) =>
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
      val f = (x: String) => if (x == "a") just("new a") else Maybe.empty[String]
      fromList(List(5 -> "a", 3 -> "b")).mapMaybe(f) must_=== singleton(5, "new a")
    }

    "mapOptionWithKey" in {
      val f = (k: Int, _: String) => if (k < 5) just("key : " + k.toString) else Maybe.empty[String]
      fromList(List(5 -> "a", 3 -> "b")).mapMaybeWithKey(f) must_=== singleton(3, "key : 3")
    }

    "mapEither" in {
      val f: String => (String \/ String) = a => if (a < "c") \/.left(a) else \/.right(a)
      val lst = fromList(List(5 -> "a", 3 -> "b", 1 -> "x", 7 -> "z"))

      lst.mapEither(f) must_===((fromList(List(3 -> "b", 5 -> "a")), fromList(List(1 -> "x", 7 -> "z"))))
      lst.mapEither((a: String) => \/-[String, String](a)) must_===((empty[Int, String], lst))
    }

    "mapEitherWithKey" in {
      val f: (Int, String) => (Int \/ String) = (k, a) => if (k < 5) \/.left(k * 2) else \/.right(a + a)
      val lst = fromList(List(5 -> "a", 3 -> "b", 1 -> "x", 7 -> "z"))

      lst.mapEitherWithKey(f) must_===(fromList(List(1 -> 2, 3 -> 6)) -> fromList(List(5 -> "aa", 7 -> "zz")))
      lst.mapEitherWithKey((_: Int, a: String) => \/-[String, String](a)) must_===(empty[Int, String] -> lst)
    }
  }

  "==>> traverse" should {
    "traverseWithKey" in {
      val f = (k: Int, v: Char) => if (k%2 == 1) Some((v + 1).toChar) else None

      empty.traverseWithKey(f) must_=== Some(empty)
      fromList(List(1 -> 'a', 5 -> 'e')).traverseWithKey(f) must_=== Some(fromList(List(1 -> 'b', 5 -> 'f')))
      fromList(List(2 -> 'a')).traverseWithKey(f) must_=== None
    }

    "traverseWithKey" ! forAll { (a: Int ==>> Char, b: Byte) =>
      val fSome = (k: Int, v: Char) => some((v + b).toChar)
      def fNone(key: Int) = (k: Int, v: Char) => if (k == key) None else some((v + b).toChar)

      val li = a.toList.map(kv => (kv._1, fSome(kv._1, kv._2).get))
      a.traverseWithKey(fSome) must_=== Some(fromList(li))

      a.toList.foreach { kv =>
        a.traverseWithKey(fNone(kv._1)) must_=== None
      }
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

    "foldMapWithKey" ! forAll { (a: Byte ==>> Byte) =>
      val f = (i: Byte, j: Byte) => i.toInt + j.toInt
      val res = a.toList.foldLeft(0: Int)((acc, kv) => acc + kv._1.toInt + kv._2.toInt)

      a.foldMapWithKey(f)(intInstance) must_=== res
    }
  }

  "==>> trim" should {
    "trim sound" ! forAll { (a: Int ==>> Int) =>
      def checkValidity(a: Int ==>> Int, lo: Maybe[Int], hi: Maybe[Int], result: Int ==>> Int) = {
        val m = trim(lo, hi, a)
        structurallySound(m)
        m must_=== result
      }

      a match {
        case Tip() =>
          val lo = Random.nextInt()
          val hi = lo + 1
          checkValidity(a, just(lo), just(hi), Tip())
          checkValidity(a, just(lo), Maybe.empty    , Tip())
          checkValidity(a, Maybe.empty    , just(hi), Tip())
          checkValidity(a, Maybe.empty    , Maybe.empty    , Tip())

        case Bin(_, _, _, _) =>
          def rec(m: Int ==>> Int): Unit = {
            m match {
              case Tip() =>
                ()
              case Bin(k, x, l, r) =>
                checkValidity(m, just(k), Maybe.empty   , r)
                checkValidity(m, Maybe.empty   , just(k), l)
                checkValidity(m, Maybe.empty   , Maybe.empty   , m)

                if (k == Int.MinValue) {
                  checkValidity(m, just(k), just(k+1), Tip())
                  checkValidity(m, Maybe.empty   , just(k+1), m)

                  rec(r)
                }
                else if (k == Int.MaxValue) {
                  checkValidity(m, just(k-1), just(k), Tip())
                  checkValidity(m, just(k-1), Maybe.empty   , m)

                  rec(l)
                }
                else {
                  checkValidity(m, just(k-1), just(k+1), m)
                  checkValidity(m, just(k-1), Maybe.empty     , m)
                  checkValidity(m, Maybe.empty     , just(k+1), m)

                  rec(l)
                  rec(r)
                }
            }
          }
        rec(a)
      }
    }

    "trimLookupLo sound" ! forAll { (a: Int ==>> Int) =>
      def checkValidity(a: Int ==>> Int, lk: Int, hk: Maybe[Int]) = {
        val (x, m) = trimLookupLo(lk, hk, a)
        structurallySound(m)

        val t1 = a.lookup(lk)
        val t2 = trim(just(lk), hk, a)
        (x, m) must_=== (t1 -> t2)
      }

      a match {
        case Tip() =>
          val lk = Random.nextInt()
          val hk = lk + 1
          checkValidity(a, lk, just(hk))
          checkValidity(a, lk, Maybe.empty)

        case Bin(_, _, _, _) =>
          def rec(m: Int ==>> Int): Unit = {
            m match {
              case Tip() =>
                ()
              case Bin(k, x, l, r) =>
                checkValidity(m, k, Maybe.empty)

                if (k == Int.MinValue) {
                  checkValidity(m, k, just(k+1))

                  rec(r)
                }
                else if (k == Int.MaxValue) {
                  checkValidity(m, k-1, just(k))
                  checkValidity(m, k-1, Maybe.empty)

                  rec(l)
                }
                else {
                  checkValidity(m, k-1, just(k+1))
                  checkValidity(m, k-1, Maybe.empty)

                  rec(l)
                  rec(r)
                }
            }
          }
        rec(a)
      }
    }
  }

  "==>> list operations" should {
    "values" in {
      fromList(List(5 -> "a", 3 -> "b")).values must_===(IList("b", "a"))
    }

    "keys" in {
      fromList(List(5 -> "a", 3 -> "b")).keys must_===(IList(3, 5))
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

  "==>> fromSet" should {
    "fromSet" in {
      fromSet(ISet.fromList(List[Int](3, 5))){ (i: Int) => List.fill(i)('a').mkString } must_=== fromList(List(5 -> "aaaaa", 3 -> "aaa"))
      fromSet(ISet.fromList(List[Int]())){ (i: Int) => i } must_=== empty
    }

    "fromSet" ! forAll { (a: ISet[Int]) =>
      val li = a.toList.map(i => (i, i))
      fromSet(a)(i => i) must_=== fromList(li)
    }

    "consistent keySet" ! forAll { (a: Byte ==>> Byte) =>
      fromSet(a.keySet)(_ => ()) must_=== a.map(_ => ())
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
  checkAll(semilattice.laws[Int ==>> ISet[Int]])

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
    val F = Align[Int ==>> *]
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
    x.filter(_.isBoth) must_=== a.filterWithKey((k, _) => keysB.member(k)).mapWithKey((k, v) => Both(v, b.lookup(k).toOption.get))
  }

  type IntMap[A] = Int ==>> A
  checkAll(traverse.laws[IntMap])
  checkAll(bind.laws[IntMap])
  checkAll(align.laws[IntMap])
  checkAll(zip.laws[IntMap])
  checkAll(bifoldable.laws[==>>])
  checkAll(FoldableTests.anyAndAllLazy[IntMap])

  object instances {
    def bind[A: Order] = Bind[A ==>> *]
    def traverse[A] = Traverse[A ==>> *]
    def band[A: Order, B: Band] = Band[A ==>> B]
    def semiLattice[A: Order, B: SemiLattice] = SemiLattice[A ==>> B]
    def monoid[A: Order, B: Semigroup] = Monoid[A ==>> B]

    // checking absence of ambiguity
    def functor[A: Order] = Functor[A ==>> *]
    def semigroup[A: Order, B: SemiLattice] = Semigroup[A ==>> B]
  }
}
