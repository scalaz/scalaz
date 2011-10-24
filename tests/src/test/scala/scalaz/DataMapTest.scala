package scalaz

class DataMapTest extends Spec {
  import org.scalacheck.{Arbitrary, Gen}
  import scalaz.scalacheck.ScalazProperties._
  import scalaz.scalacheck.ScalazProperties._
  import scalaz.scalacheck.ScalazArbitrary._
  import std.anyVal._
  import std.list._
  import std.string._
  import std.option._
  import syntax.equal._
  import syntax.show._
  import syntax.std.option._

  import ==>>._

  "==>> fromList" should {
    "succeed" in {
      fromList(List.empty[(Int, String)]) === empty[Int, String]
      fromList(List(5 -> "a", 3 -> "b")) === fromList(List(3 -> "b", 5 -> "a"))
      fromList(List(5 ->"a", 3 -> "b", 5 -> "c")) === fromList(List(5 -> "c", 3 -> "b"))
      fromList(List(5 -> "c", 3 -> "b", 5 -> "a")) === fromList(List(5 -> "a", 3 -> "b"))

      Equal[Int ==>> String].equal(fromList(List(5 -> "a", 3 -> "b")), fromList(List(3 -> "b", 5 -> "a")))
    }
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
  }

  "==>> conversions" should {
    "toAscList" in {
      import std.tuple._
      implicit val listTupleShow = Show[List[(Int, String)]]

      fromList(List(5 -> "a", 3 -> "b")).toAscList must be_===(fromList(List(3 -> "b", 5 -> "a")).toAscList)
      fromList(List(5 -> "a", 7 -> "c", 3 -> "b")) must be_===(fromList(List(3 -> "b", 5 -> "a", 7 -> "c")))
    }
  }

  "==>> lookups" should {
    val d = fromList(List(5 -> "a", 3 -> "b"))

    "value lookup" in {
      fromList(List(("John","Sales"), ("Bob","IT"))).lookup("John") must be_===(Some("Sales"))
      fromList(List(("John","Sales"), ("Bob","IT"))).lookup("Sarah") must be_===(None)
    }

    "index lookup" in {
      d.lookupIndex(2).isDefined must_== false
      d.lookupIndex(3).get must_== 0
      d.lookupIndex(5).get must_== 1
      d.lookupIndex(6).isDefined must_== false
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

      d.updateAt(0, (_, _) => "x".some) must be_===(fromList(List(3 -> "x", 5 -> "a")))
      d.updateAt(1, (_, _) => "x".some) must be_===(fromList(List(3 -> "b", 5 -> "x")))
      //d.updateAt(2, (_, _) => "x".some) must be_===(empty[Int, String])

      d.updateAt(0, (_, _) => None) must be_===(singleton(5, "a"))
      d.updateAt(1, (_, _) => None) must be_===(singleton(3, "b"))
    }
  }

  "delete" should {
    "remove an element" in {
      fromList(List((5,"a"), (3,"b"))).delete(5) must be_===(singleton(3, "b"))
    }
    "not remove an element" in {
      fromList(List((5,"a"), (3,"b"))).delete(7) must be_===(fromList(List((5,"a"), (3,"b"))))
    }
    "not remove from an empty map" in {
      empty.delete(5) must be_===(empty[Int, Int])
    }
  }

  "==>> insertion" should {
    "insert" in {
      fromList(List(5 -> "a", 3 -> "b")).insert(5, "x") === fromList(List(3 -> "b", 5 -> "x")) // Replacement
      fromList(List((5,"a"), (3,"b"))).insert(7,"x") must be_===(fromList(List((3,"b"), (5,"a"), (7,"x")))) // Addition of new key
      empty.insert(5, "x") must be_===(singleton(5, "x"))
    }

    "insertWith" in {
      val r = fromList(List(5 -> "a", 3 -> "b")).insertWith(_ + _, 5, "xxx")
      r must be_===(fromList(List(3 -> "b", 5 -> "xxxa")))

      fromList(List(5 -> "a", 3 -> "b")).insertWith(_ + _, 7, "xxx") must be_===(fromList(List(3 -> "b", 5 -> "a", 7 -> "xxx")))
      empty[Int, String].insertWith(_ + _, 5, "xxx") must be_===(singleton(5, "xxx"))
    }

    "insertWithKey" in {
      val f = (k: Int, a: String, b: String) => k.toString + ":" + a + "|" + b
      fromList(List(5 -> "a", 3 -> "b")).insertWithKey(f, 5, "xxx") must be_===(fromList(List(3 -> "b", 5 -> "5:xxx|a")))
      fromList(List(5 -> "a", 3 -> "b")).insertWithKey(f, 7, "xxx") must be_===(fromList(List(3 -> "b", 5 -> "a", 7 -> "xxx")))
      empty.insertWithKey(f, 5, "xxx") must be_===(singleton(5, "xxx"))
    }
  }

  "==>> from a list" should {
    "equivalence to insert on empty ==>>" in {
      empty.insert(2,3).insert(3,4) must be_===(fromList(List(2 -> 3, 3 -> 4)))
    }

    "create a valid map from empty list" in {
      fromList(List.empty[(Int, Int)]) must be_===(empty[Int, Int])
    }
  }

  "==>> union operations" should {
    "union" in {
      fromList(List((5, "a"), (3, "b"))) union fromList(List((5, "A"), (7, "C"))) must_== fromList(List((3, "b"), (5, "a"), (7, "C")))
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

    "syntax" in {
      val r = fromList(List(5 -> "a", 3 -> "b")) \\ fromList(List(5 -> "A", 7 -> "C"))
      r must_== singleton[Int, String](3, "b")
    }

    "differenceWith" in {
      val f = (al: String, ar: String) => if (al == "b") Some(al + ":" + ar) else None
      fromList(List(5 -> "a", 3 -> "b")).differenceWith(fromList(List(5 -> "A", 3 -> "B", 7 -> "C")), f) must be_===(singleton(3, "b:B"))
    }

    "differenceWithKey" in {
      val f = (k: Int, al: String, ar: String) => if (al == "b") Some(k.toString + ":" + al + "|" + ar) else None
      fromList(List(5 -> "a", 3 -> "b")).differenceWithKey(fromList(List(5 -> "A", 3 -> "B", 10 -> "C")), f) must be_===(singleton(3, "3:b|B"))
    }
  }

  "==>> intersection operations" should {
    "intersection" in {
      val r = fromList(List(5 -> "a", 3 -> "b")) intersection fromList(List(5 -> "A", 7 -> "C"))
      r must_== singleton(5, "a")
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

      fromList(List(5 -> "a", 3 -> "b")).adjust(5, f) must be_===(fromList(List(3 -> "b", 5 -> "new a")))
      fromList(List(5 -> "a", 3 -> "b")).adjust(7, f) must be_===(fromList(List(3 -> "b", 5 -> "a")))
      empty[Int, String].adjust(7, f) must be_===(empty[Int, String])
    }

    "adjustWithKey" in {
      val f = (k: Int, x: String) => k.toString + ":new " + x

      fromList(List(5 -> "a", 3 -> "b")).adjustWithKey(5, f) must be_===(fromList(List(3 -> "b", 5 -> "5:new a")))
      fromList(List(5 -> "a", 3 -> "b")).adjustWithKey(7, f) must be_===(fromList(List(3 -> "b", 5 -> "a")))
      empty[Int, String].adjustWithKey(7, f) must be_===(empty[Int, String])
    }

    "update" in {
      val f = (x: String) => if (x == "a") Some("new a") else None

      fromList(List(5 -> "a", 3 -> "b")).update(5, f) must be_===(fromList(List(3 -> "b", 5 -> "new a")))
      fromList(List(5 -> "a", 3 -> "b")).update(7, f) must be_===(fromList(List(3 -> "b", 5 -> "a")))
      fromList(List(5 -> "a", 3 -> "b")).update(3, f) must be_===(singleton(5, "a"))
    }

    "updateWithKey" in {
      val f = (k: Int, x: String) => if (x == "a") Some(k.toString + ":new a") else None

      fromList(List(5 -> "a", 3 -> "b")).updateWithKey(5, f) must be_===(fromList(List(3 -> "b", 5 -> "5:new a")))
      fromList(List(5 -> "a", 3 -> "b")).updateWithKey(7, f) must be_===(fromList(List(3 -> "b", 5 -> "a")))
      fromList(List(5 -> "a", 3 -> "b")).updateWithKey(3, f) must be_===(singleton(5, "a"))
    }

    "updateLookupWithKey" in {
      import std.tuple._
      val f = (k: Int, x: String) => if (x == "a") Some(k.toString + ":new a") else None

      fromList(List(5 -> "a", 3 -> "b")).updateLookupWithKey(5, f) must be_===((Some("5:new a"), fromList(List(3 -> "b", 5 -> "5:new a"))))
      fromList(List(5 -> "a", 3 -> "b")).updateLookupWithKey(7, f) must be_===((None, fromList(List(3 -> "b", 5 -> "a"))))
      fromList(List(5 -> "a", 3 -> "b")).updateLookupWithKey(3, f) must be_===((Some("b"), singleton(5, "a")))
    }

    "alter" in {
      val f1 = (_: Option[String]) => none[String]
      fromList(List(5 -> "a", 3 -> "b")).alter(7, f1) must be_===(fromList(List(3 -> "b", 5 -> "a")))
      fromList(List(5 -> "a", 3 -> "b")).alter(5, f1) must be_===(singleton(3, "b"))

      val f2 = (_: Option[String]) => "c".some
      fromList(List(5 -> "a", 3 -> "b")).alter(7, f2) must be_===(fromList(List(3 -> "b", 5 -> "a", 7 -> "c")))
      fromList(List(5 -> "a", 3 -> "b")).alter(5, f2) must be_===(fromList(List(3 -> "b", 5 -> "c")))
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
  }

  "==>> filter" should {
    val m = fromList(List(5 -> "a", 3 -> "b"))

    "filter" in {
      m.filter(_ > "a") must be_===(singleton(3, "b"))
      m.filter(_ > "x") must be_===(empty[Int, String])
      m.filter(_ < "a") must be_===(empty[Int, String])
    }
  }

  "==>> partition" should {
    val m = fromList(List(5 -> "a", 3 -> "b"))

    "partition" in {
      m.partition(_ > "a") === (singleton(3, "b"), singleton(5, "a"))
      m.partition(_ < "x") === (fromList(List(3 -> "b", 5 -> "a")), empty[Int, String])
      m.partition(_ > "x") === (empty[Int, String], fromList(List(3 -> "b", 5 -> "a")))
    }
  }

  "==>> map" should {
    import std.tuple._

    "map" in {
      fromList(List(5 -> "a", 3 -> "b")).map(_ + "x") must be_===(fromList(List(3 -> "bx", 5 -> "ax")))
    }

    "mapWithKey" in {
      val f = (k: Int, x: String) => k.toString + ":" + x
      fromList(List(5 -> "a", 3 -> "b")).mapWithKey(f) must be_===(fromList(List(3 -> "3:b", 5 -> "5:a")))
    }

    "mapAccum" in {
      val f = (a: String, b: String) => (a + b, b + "X")
      fromList(List(5 -> "a", 3 -> "b")).mapAccum("Everything: ")(f) must be_===("Everything: ba", fromList(List(3 -> "bX", 5 -> "aX")))
    }

    "mapAccumWithKey" in {
      val f = (a: String, k: Int, b: String) => (a + " " + k.toString + "-" + b, b + "X")
      fromList(List(5 -> "a", 3 -> "b")).mapAccumWithKey("Everything:")(f) must be_===("Everything: 3-b 5-a", fromList(List(3 -> "bX", 5 -> "aX")))
    }

    "mapKeys" in {
      fromList(List(5 -> "a", 3 -> "b")).mapKeys(_ + 1) must be_===(fromList(List(4 -> "b", 6 -> "a")))
      fromList(List(1 -> "b", 2 -> "a", 3 -> "d", 4 -> "c")).mapKeys(_ => 1) must be_===(singleton(1, "c"))
      fromList(List(1 -> "b", 2 -> "a", 3 -> "d", 4 -> "c")).mapKeys(_ => 3) must be_===(singleton(3, "c"))
    }

    "mapWithKeys" in {
      fromList(List(1 -> "b", 2 -> "a", 3 -> "d", 4 -> "c")).mapKeysWith(_ => 1, _ + _) must be_===(singleton(1, "cdab"))
      fromList(List(1 -> "b", 2 -> "a", 3 -> "d", 4 -> "c")).mapKeysWith(_ => 3, _ + _) must be_===(singleton(3, "cdab"))
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
      fromList(List(5 -> "a", 3 -> "b")).values must be_===(List("b", "a"))
    }

    "keys" in {
      fromList(List(5 -> "a", 3 -> "b")).keys must be_===(List(3, 5))
    }

    "keySet" in {
      import std.set._

      fromList(List(5 -> "a", 3 -> "b")).keySet must be_===(Set(3, 5))
      empty[Int, String].keySet must be_===(Set.empty[Int])
    }

    "fromList" in {
      fromList(List.empty[(Int, String)]) must be_===(empty[Int, String])
      fromList(List(5 -> "a", 3 -> "b", 5 -> "c")) must be_===(fromList(List(5 -> "c", 3 -> "b")))
      fromList(List(5 -> "c", 3 -> "b", 5 -> "a")) must be_===(fromList(List(5 -> "a", 3 -> "b")))
    }

    "fromListWith" in {
      fromListWith(List(5 -> "a", 5 -> "b", 3 -> "b", 3 -> "a", 5 -> "a"))(_ + _) must be_===(fromList(List(3 -> "ab", 5 -> "aba")))
      fromListWith(List.empty[(Int, String)])(_ + _) must be_===(empty[Int, String])
    }

    "fromListWithKey" in {
      val f = (k: Int, a1: String, a2: String) => k.toString + a1 + a2

      fromListWithKey(List(5 -> "a", 5 -> "b", 3 -> "b", 3 -> "a", 5 -> "a"))(f) must be_===(fromList(List(3 -> "3ab", 5 -> "5a5ba")))
      fromListWithKey(List.empty[(Int, String)])(f) must be_===(empty[Int, String])
    }

    "toList" in {
      import std.tuple._
      fromList(List(5 -> "a", 3 -> "b")).toList must be_===(List(3 -> "b", 5 -> "a"))
      empty[Int, String].toList must be_===(List.empty[(Int, String)])
    }
  }

  /*"==>> validity" should {
    "valid" in {
      fromList(List(3 -> "b", 5 -> "a")).isValid must_== true
      //-- > valid (fromAscList [(3,"b"), (5,"a")]) == True
      //-- > valid (fromAscList [(5,"a"), (3,"b")]) == False
    }
  }*/

  import scalaz.scalacheck.ScalaCheckBinding._

  implicit def mapArb[A: Order, B](implicit A: Arbitrary[List[(A, B)]]): Arbitrary[A ==>> B] =
    Functor[Arbitrary].map(A)(as => fromList(as))

  checkAll(equal.laws[Int ==>> Int])
  checkAll(order.laws[Int ==>> Int])

  type IntMap[A] = Int ==>> A
  checkAll(functor.laws[IntMap])
  checkAll(traverse.laws[IntMap])
}
