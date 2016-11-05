package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import Id._
import org.scalacheck.Prop.forAll

object ListTest extends SpecLite {
  checkAll(equal.laws[List[Int]])
  checkAll(monoid.laws[List[Int]])
  checkAll(bindRec.laws[List])
  checkAll(monadPlus.strongLaws[List])
  checkAll(traverse.laws[List])
  checkAll(zip.laws[List])
  checkAll(align.laws[List])
  checkAll(isEmpty.laws[List])
  checkAll(cobind.laws[List])
  checkAll(order.laws[List[Int]])

  import std.list.listSyntax._
  import syntax.foldable._

  "intercalate empty list is flatten" ! forAll((a: List[List[Int]]) => a.intercalate(List[Int]()) must_===(a.flatten))

  "intersperse then remove odd items is identity" ! forAll {
    (a: List[Int], b: Int) =>
      val isEven = (_: Int) % 2 == 0
      a.intersperse(b).zipWithIndex.filter(p => isEven(p._2)).map(_._1) must_===(a)
  }

  "intercalate is same as a.intersperse(b).flatten" ! forAll {
    (a: List[List[Int]], b: List[Int]) =>
      a.intercalate(b) must_===(a.intersperse(b).flatten)
  }

  "intersperse vs benchmark" ! forAll {
    def intersperse[A](value: List[A], a: A): List[A] = value match {
      case Nil      => Nil
      case x :: Nil => x :: Nil
      case h :: t   => h :: a :: intersperse(t, a)
    }
    (a: List[Int], b: Int) => (a.intersperse(b) must_===(intersperse(a, b)))
  }

  "groupWhenM[Id].flatten is identity" ! forAll {
    (a: List[Int], p: (Int, Int) => Boolean) =>
      a.groupWhenM[Id](p).map(_.list.toList).flatten must_===(a)
  }

  "groupByWhenM[Id] ∀(i,j) | 0<i<resut.len & 0<j<result(i).len: p(result(i)(j), p(result(i)(j+1)) yields true" ! forAll {
    (a: List[Int], p: (Int, Int) => Boolean) =>
      a.groupWhenM[Id](p).forall { group: NonEmptyList[Int] =>
        list.adjacentPairs(group.list.toList).forall(p.tupled)
      }
  }

  "groupByWhenM[Id] ∀ i | 0<i<result.len: p(result(i).last, result(i+1).head) yields false" ! forAll {
    (a: List[Int], p: (Int, Int) => Boolean) =>
      val pairs = list.adjacentPairs(a.groupWhen(p))
      pairs.forall {
        case (l, r) => !p(l.last, r.head)
      }
  }

  "groupBy1" ! forAll {
      (a: List[String]) =>
      val strlen = (_ : String).length
      (a groupBy strlen) must_===((a groupBy1 strlen) mapValues (_.list.toList))
  }

  "groupWhen.flatten is identity" ! forAll {
    (a: List[Int], p: (Int, Int) => Boolean) =>
      a.groupWhen(p).map(_.list.toList).flatten must_===(a)
  }

  "filterM" ! forAll {
    (xs: List[Int]) => xs.filterM[Id](_ % 2 == 0) == xs.filter(_ % 2 == 0)
  }

  "filter consistent with fiterM[Id]" ! forAll {
    (xs: List[Int], p: Int => Boolean) => MonadPlus[List].filter(xs)(p) must_=== xs.filterM[Id](p)
  }

  "groupWhen.flatten is identity" ! forAll {
    (a: List[Int], p: (Int, Int) => Boolean) =>
      a.groupWhen(p).map(_.list.toList).flatten must_===(a)
  }

  "groupByWhen ∀(i,j) | 0<i<resut.len & 0<j<result(i).len: p(result(i)(j), p(result(i)(j+1)) yields true" ! forAll {
    (a: List[Int], p: (Int, Int) => Boolean) =>
      a.groupWhen(p).forall { group: NonEmptyList[Int] =>
        list.adjacentPairs(group.list.toList).forall(p.tupled)
      }
  }

  "groupByWhen ∀ i | 0<i<result.len: p(result(i).last, result(i+1).head) yields false" ! forAll {
    (a: List[Int], p: (Int, Int) => Boolean) =>
      val pairs = list.adjacentPairs(a.groupWhen(p))
      pairs.forall {
        case (l, r) => !p(l.last, r.head)
      }
  }

  "lookups in assoc lists sometime return a value" ! forAll {
    (a: List[(Int, Int)]) => {
      a.headOption match {
        case None    => a.lookup[Int, Int](0)    must_===(None)
        case Some(x) => a.lookup[Int, Int](x._1) must_===(Some(x._2))
      }
    }
  }

  "takeWhileM example" in {
    def takeWhileN[A](as: List[A], n: Int)(f: A => Boolean): List[A] = as.takeWhileM[State[Int, ?]](a => State {
      i =>
        val j = i + (if (f(a)) 0 else 1)
        val done = j >= n
        (j, !done)
    }).evalZero[Int]

    val actual = takeWhileN("/abc/def/hij/klm".toList, 4)(_ != '/').mkString
    actual must_===("/abc/def/hij")
  }

  "foldl is foldLeft" ! forAll {(rnge: List[List[Int]]) =>
    val F = Foldable[List]
    (rnge.foldLeft(List[Int]())(_++_)
      must_===(F.foldLeft(rnge, List[Int]())(_++_)))
  }

  "foldr is foldRight" ! forAll {(rnge: List[List[Int]]) =>
    val F = Foldable[List]
    (rnge.foldRight(List[Int]())(_++_)
      must_===(F.foldRight(rnge, List[Int]())(_++_)))
  }

  "index" ! forAll { (xs: List[Int], n: Int) =>
    (xs index n) must_===(if (n >= 0 && xs.size > n) Some(xs(n)) else None)
  }

  "groupWhen is groupWhenM[Id]" ! forAll { xs: List[Int] =>
    val f: (Int, Int) => Boolean = _ > _
    xs.groupWhen(f) must_=== xs.groupWhenM[Id.Id](f)
  }

  "mapAccumLeft" ! forAll {
    (xs: List[Int]) =>
      val f = (_: Int) + 1
      xs.mapAccumLeft(List[Int](), (c: List[Int], a) =>
        (c :+ a, f(a))) must_===(xs, xs.map(f))
  }

  "mapAccumRight" ! forAll {
    (xs: List[Int]) =>
      val f = (_: Int) + 1
      xs.mapAccumRight(List[Int](), (c: List[Int], a) =>
        (c :+ a, f(a))) must_===(xs.reverse, xs.map(f))
  }

  checkAll(FoldableTests.anyAndAllLazy[List])


  object instances {
    def equal[A: Equal] = Equal[List[A]]
    def order[A: Order] = Order[List[A]]
    def monoid[A] = Monoid[List[A]]
    def bindRec = BindRec[List]
    def monadPlus = MonadPlus[List]
    def traverse = Traverse[List]
    def zip = Zip[List]
    def unzip = Unzip[List]
    def align = Align[List]
    def isEmpty = IsEmpty[List]
    def cobind = Cobind[List]

    // checking absence of ambiguity
    def equal[A: Order] = Equal[Option[A]]
  }
}
