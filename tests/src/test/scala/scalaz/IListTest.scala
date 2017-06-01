package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll
import syntax.bifunctor._, syntax.foldable._

object IListTest extends SpecLite {

  // Same laws that hold for List
  checkAll(equal.laws[IList[Int]])
  checkAll(monoid.laws[IList[Int]])
  checkAll(monadPlus.strongLaws[IList])
  checkAll(bindRec.laws[IList])
  checkAll(traverse.laws[IList])
  checkAll(zip.laws[IList])
  checkAll(align.laws[IList])
  checkAll(isEmpty.laws[IList])
  checkAll(cobind.laws[IList])
  checkAll(order.laws[IList[Int]])

  // These tests hold for List, so they had better hold for IList

  "intercalate empty list is flatten" ! forAll { (a: IList[IList[Int]]) =>
    a.intercalate(IList[Int]()) must_===(a.flatten)
  }

  "intersperse then remove odd items is identity" ! forAll { (a: IList[Int], b: Int) =>
    val isEven = (_: Int) % 2 == 0
    a.intersperse(b).zipWithIndex.filter(p => isEven(p._2)).map(_._1) must_===(a)
  }

  "intercalate is same as a.intersperse(b).flatten" ! forAll { (a: IList[IList[Int]], b: IList[Int]) =>
    a.intercalate(b) must_===(a.intersperse(b).flatten)
  }

  "intersperse vs benchmark" ! forAll { (a: IList[Int], b: Int) =>
    def intersperse[A](value: IList[A], a: A): IList[A] = value match {
      case INil() => INil()
      case ICons(x, INil()) => x :: INil()
      case ICons(h, t) => h :: a :: intersperse(t, a)
    }
    a.intersperse(b) must_=== intersperse(a, b)
  }

  "foldl is foldLeft" ! forAll {(rnge: IList[IList[Int]]) =>
    val F = Foldable[List]
    rnge.foldLeft(IList[Int]())(_++_) must_=== F.foldLeft(rnge.toList, IList[Int]())(_++_)
  }

  "foldr is foldRight" ! forAll {(rnge: IList[IList[Int]]) =>
    val F = Foldable[List]
    rnge.foldRight(IList[Int]())(_++_) must_=== F.foldRight(rnge.toList, IList[Int]())(_++_)
  }

  "foldLeft1Opt" ! forAll { ns: IList[List[Int]] =>
    ns.foldLeft1Opt(_ ::: _) must_=== ns.toList.reduceLeftOption(_ ::: _)
  }

  "foldRight1Opt" ! forAll { ns: IList[List[Int]] =>
    ns.foldRight1Opt(_ ::: _) must_=== ns.toList.reduceRightOption(_ ::: _)
  }

  "foldMap1Opt" ! forAll { ns: IList[List[Int]] =>
    ns.foldMap1Opt(identity) must_=== ns.toList.reduceLeftOption(_ ::: _)
  }

  "mapAccumLeft" ! forAll { xs: IList[Int] =>
    val f = (_: Int) + 1
    xs.mapAccumLeft(IList[Int]())((c, a) => (c :+ a, f(a))) must_=== (xs, xs.map(f))
  }

  "mapAccumRight" ! forAll { xs: IList[Int] =>
    val f = (_: Int) + 1
    xs.mapAccumRight(IList[Int]())((c, a) => (c :+ a, f(a))) must_=== (xs.reverse, xs.map(f))
  }

  // And some other tests that List doesn't have

  "catamorphism" ! forAll { (ns: IList[Int]) =>
    ns.foldRight(IList.empty[Int])(ICons(_, _)) must_=== ns
  }

  // Functionality borrowed from List is tested in terms of List. Is this ethical?
  // Should they be collapsed into fewer cases?

  "++" ! forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns ++ ms).toList must_=== ns.toList ++ ms.toList
  }

  "++:" ! forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns ++: ms).toList must_=== ns.toList ++: ms.toList
  }

  "+:" ! forAll { (n: Int, ns: IList[Int]) =>
    (n +: ns).toList must_=== n +: ns.toList
  }

  "/:" ! forAll { (ns: IList[Int], s: String, f: (String, Int) => String) =>
    (s /: ns)(f) == (s /: ns.toList)(f)
  }

  ":+" ! forAll { (n: Int, ns: IList[Int]) =>
    (ns :+ n).toList must_=== ns.toList :+ n
  }

  "::" ! forAll { (n: Int, ns: IList[Int]) =>
    (n :: ns).toList must_=== n :: ns.toList
  }

  ":::" ! forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns ::: ms).toList must_=== ns.toList ::: ms.toList
  }

  ":\\" ! forAll { (ns: IList[Int], s: String, f: (Int, String) => String) =>
    (ns :\ s)(f) == (ns.toList :\ s)(f)
  }

  "concat" ! forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns concat ms).toList must_=== ns.toList ++ ms.toList
  }

  "collect" ! forAll { (ns: IList[Int]) =>
    val pf: PartialFunction[Int, Int] = { case n if n % 2 == 0 => n + 1 }
    ns.collect(pf).toList must_=== ns.toList.collect(pf)
  }

  "collectFirst" ! forAll { (ns: IList[Int]) =>
    val pf: PartialFunction[Int, Int] = { case n if n % 2 == 0 => n + 1 }
    ns.collectFirst(pf) must_=== ns.toList.collectFirst(pf)
  }

  "concat" ! forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns ++ ms).toList must_=== ns.toList ++ ms.toList
  }

  "containsSlice" ! forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.containsSlice(ms) must_=== ns.toList.containsSlice(ms.toList)
  }

  "count" ! forAll { (ns: IList[Int], p: Int => Boolean) =>
    ns.count(p) must_=== ns.toList.count(p)
  }

  "distinct" ! forAll { xs: IList[Int] =>
    xs.distinct.toList must_=== xs.toList.distinct
  }

  "drop" ! forAll { (ns: IList[Int], n: Int) =>
    ns.drop(n).toList must_=== ns.toList.drop(n)
  }

  "dropRight" ! forAll { (ns: IList[Int], n: Int) =>
    ns.dropRight(n).toList must_=== ns.toList.dropRight(n)
  }

  "dropRightWhile" ! forAll { (ns: IList[Int], p: Int => Boolean) =>
    ns.dropRightWhile(p).toList must_=== ns.toList.reverse.dropWhile(p).reverse
  }

  "dropWhile" ! forAll { (ns: IList[Int], p: Int => Boolean) =>
    ns.dropWhile(p).toList must_=== ns.toList.dropWhile(p)
  }

  "endsWith" ! forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.endsWith(ms) must_=== ns.toList.endsWith(ms.toList)
  }

  "fill" ! forAll { (a: Byte, b: Int) =>
    IList.fill(a)(b).toList must_=== List.fill(a)(b)
  }

  "filter" ! forAll { (ns: IList[Int], p: Int => Boolean) =>
    ns.filter(p).toList must_=== ns.toList.filter(p)
  }

  "filterNot" ! forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.filterNot(f).toList must_=== ns.toList.filterNot(f)
  }

  "find" ! forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.find(f) must_=== ns.toList.find(f)
  }

  // flatMap and folds are covered by laws

  "groupBy" ! forAll { (ns: IList[Int], f: Int => Int) =>
    ns.groupBy(f).map(_.toList).toList.toMap must_=== ns.toList.groupBy(f)
  }

  "groupBy1" ! forAll { (ns: IList[Int], f: Int => Int) =>
    ns.groupBy1(f).map(oa => (oa.head :: oa.tail).toList.reverse).toList.toMap must_=== ns.toList.groupBy(f)
  }

  "headOption" ! forAll { ns: IList[Int] =>
    ns.headOption must_=== ns.toList.headOption
  }

  "index" ! forAll { (ns: IList[Int], n: Int) =>
    ns.index(n) must_=== ns.toList.lift(n)
  }

  "indexOf" ! forAll { (ns: IList[Int], n: Int) =>
    ns.indexOf(n).getOrElse(-1) must_=== ns.toList.indexOf(n)
  }

  "indexOfSlice" ! forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.indexOfSlice(ms).getOrElse(-1) must_=== ns.toList.indexOfSlice(ms.toList)
  }

  "indexWhere" ! forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.indexWhere(f).getOrElse(-1) must_=== ns.toList.indexWhere(f)
  }

  "initOption" ! forAll { ns: IList[Int] =>
    ns.initOption.map(_.toList) must_=== (try Some(ns.toList.init) catch { case e: Exception => None })
  }

  "inits" ! forAll { ns: IList[Int] =>
    ns.inits.map(_.toList).toList must_=== ns.toList.inits.toList
  }

  "interleave" ! forAll { (xs: IList[Int], ys: IList[Int]) =>
    val a = xs interleave ys
    (xs.length + ys.length) must_=== a.length
    val min = math.min(xs.length, ys.length)

    Foldable[IList].all(xs.zipWithIndex){ case (x, i) =>
      val index = if(i <= min) i * 2 else (min * 2) + i - min
      a.index(index) == Some(x)
    } must_=== true

    Foldable[IList].all(ys.zipWithIndex){ case (y, i) =>
      val index = if(i < min) (i * 2) + 1 else (min * 2) + i - min
      a.index(index) == Some(y)
    } must_=== true

    xs.interleave(ys).toStream must_=== std.stream.interleave(xs.toStream, ys.toStream)
  }

  // intersperse is tested above
  // isEmpty is tested by empty laws

  "lastIndexOf" ! forAll { (ns: IList[Int], n: Int) =>
    ns.lastIndexOf(n).getOrElse(-1) must_=== ns.toList.lastIndexOf(n)
  }

  "lastIndexOfSlice" ! forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.lastIndexOfSlice(ms).getOrElse(-1) must_=== ns.toList.lastIndexOfSlice(ms.toList)
  }

  "lastIndexWhere" ! forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.lastIndexWhere(f).getOrElse(-1) must_=== ns.toList.lastIndexWhere(f)
  }

  "lastOption" ! forAll { ns: IList[Int] =>
    ns.lastOption must_=== ns.toList.lastOption
  }

  "length" ! forAll { ns: IList[Int] =>
    ns.length must_=== ns.toList.length
  }

  // map is tested by functor laws

  "nonEmpty" ! forAll { ns: IList[Int] =>
    ns.nonEmpty must_=== ns.toList.nonEmpty
  }

  "padTo" ! forAll { (ns: IList[Int], n: Int) =>
    ns.padTo(100, n).toList must_=== ns.toList.padTo(100, n)
  }

  "patch" ! forAll { (ns: IList[Int], a: Int, ms: IList[Int], b: Int) =>
    ns.patch(a, ms, b).toList must_=== ns.toList.patch(a, ms.toList, b)
  }

  "prefixLength" ! forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.prefixLength(f) must_=== ns.toList.prefixLength(f)
  }

  "reduceLeftOption" ! forAll { (ns: IList[Int], f: (Int, Int) => Int) =>
    ns.reduceLeftOption(f) must_=== (try Some(ns.toList.reduceLeft(f)) catch { case e:Exception => None })
  }

  "reduceRightOption" ! forAll { (ns: IList[Int], f: (Int, Int) => Int) =>
    ns.reduceRightOption(f) must_=== (try Some(ns.toList.reduceRight(f)) catch { case e:Exception => None })
  }

  "reverse" ! forAll { ns: IList[Int] =>
    ns.reverse.toList must_=== ns.toList.reverse
  }

  "reverseMap" ! forAll { (ns: IList[Int], f: Int => Int) =>
    ns.reverseMap(f).toList must_=== ns.toList.reverseMap(f)
  }

  "reverse_:::" ! forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns reverse_::: ms).toList must_=== (ns.toList reverse_::: ms.toList)
  }

  "scanLeft" ! forAll { (ss: IList[String], f: (Int, String) => Int) =>
    ss.scanLeft(0)(f).toList must_=== ss.toList.scanLeft(0)(f)
    ss.scanLeft("z")(_ + _).toList must_=== ss.toList.scanLeft("z")(_ + _)
    ss.scanLeft(IList.empty[String])(_ :+ _).toList must_=== ss.toList.scanLeft(IList.empty[String])(_ :+ _)
  }

  "scanRight" ! forAll { (ss: IList[String], f: (String, Int) => Int)  =>
    ss.scanRight(0)(f).toList must_=== ss.toList.scanRight(0)(f)
    ss.scanRight("z")(_ + _).toList must_=== ss.toList.scanRight("z")(_ + _)
    ss.scanRight(IList.empty[String])(_ +: _).toList must_=== ss.toList.scanRight(IList.empty[String])(_ +: _)
  }

  "slice" ! forAll { (ns: IList[Int], a: Int, b: Int) =>
    ns.slice(a, b).toList must_=== ns.toList.slice(a, b)
  }

  "sortBy" ! forAll { (ss: IList[String], f: String => Int) =>
    ss.sortBy(f).toList must_=== ss.toList.sortBy(f)
  }

  "sorted" ! forAll { (ss: IList[String]) =>
    ss.sorted.toList must_=== ss.toList.sorted
  }

  "span" ! forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.span(f).umap(_.toList) must_=== ns.toList.span(f)
  }

  "splitAt" ! forAll { (ns: IList[Int], n: Int) =>
    ns.splitAt(n).umap(_.toList) must_=== ns.toList.splitAt(n)
  }

  "startsWith" ! forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.startsWith(ms) must_=== ns.toList.startsWith(ms.toList)
  }

  "tails" ! forAll { ns: IList[Int] =>
    ns.tails.map(_.toList).toList must_=== ns.toList.tails.toList
  }

  "tailOption" ! forAll { ns: IList[Int] =>
    ns.tailOption.map(_.toList) must_=== (try Some(ns.toList.tail) catch { case e: Exception => None })
  }

  "take" ! forAll { (ns: IList[Int], n: Byte) =>
    ns.take(n).toList must_=== ns.toList.take(n)
  }

  "takeRight" ! forAll { (ns: IList[Int], n: Byte) =>
    ns.takeRight(n).toList must_=== ns.toList.takeRight(n)
  }

  "takeRightWhile" ! forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.takeRightWhile(f).toList must_=== ns.toList.reverse.takeWhile(f).reverse
  }

  "takeWhile" ! forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.takeWhile(f).toList must_=== ns.toList.takeWhile(f)
  }

  "toEphemeralStream" ! forAll { ns: List[Int] =>
    IList(ns: _*).toEphemeralStream.toList must_=== EphemeralStream(ns: _*).toList
  }

  "toList" ! forAll { ns: List[Int] =>
    IList(ns: _*).toList must_=== ns
  }

  "toMap" ! forAll { ps: List[(String, Int)] =>
    IList(ps: _*).toMap must_=== ==>>(ps: _*)
  }

  "toNel" ! forAll { ns: List[Int] =>
    IList(ns: _*).toNel must_=== Scalaz.ToListOpsFromList(ns).toNel
  }

  "toStream" ! forAll { ns: List[Int] =>
    IList(ns: _*).toStream must_=== ns.toStream
  }

  "toVector" ! forAll { ns: Vector[Int] =>
    IList(ns: _*).toVector must_=== ns
  }

  "toZipper" ! forAll { ns: List[Int] =>
    IList(ns: _*).toZipper must_=== scalaz.std.stream.toZipper(ns.toStream)
  }

  // uncons is tested everywhere

  "updated" ! forAll { (ns: IList[Int], i: Int, n: Int) =>
    if (i < 0 || i >= ns.length) {
      ns.updated(i, n) must_=== ns
    } else {
      ns.updated(i, n).toList must_=== ns.toList.updated(i, n)
    }
  }

  "unzip" ! forAll { (ns: IList[(Int, String)]) =>
    ns.unzip.bimap(_.toList, _.toList) must_=== ns.toList.unzip
  }

  // widen is tested by toMap and unzip
  // zip is tested by zip laws

  "zipWithIndex" ! forAll { ns: IList[Int] =>
    ns.zipWithIndex.toList must_=== ns.toList.zipWithIndex
  }

  checkAll(FoldableTests.anyAndAllLazy[IList])

  object instances {
    def equal[A: Equal] = Equal[IList[A]]
    def order[A: Order] = Order[IList[A]]
    def monoid[A] = Monoid[IList[A]]
    def monadPlus = MonadPlus[IList]
    def bindrec = BindRec[IList]
    def traverse = Traverse[IList]
    def zip = Zip[IList]
    def align = Align[IList]
    def isEmpty = IsEmpty[IList]
    def cobind = Cobind[IList]
  }
}
