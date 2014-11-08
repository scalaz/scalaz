package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.{Arbitrary, Prop, Gen}
import org.scalacheck.Prop.forAll
import syntax.bifunctor._, syntax.foldable._

object IVectorTest extends SpecLite {

  // Same laws that hold for Vector
  checkAll(equal.laws[IVector[Int]])
  checkAll(monoid.laws[IVector[Int]])
  checkAll(monadPlus.strongLaws[IVector])
  checkAll(traverse.laws[IVector])
  checkAll(zip.laws[IVector])
  checkAll(align.laws[IVector])
  checkAll(isEmpty.laws[IVector])
  checkAll(cobind.laws[IVector])
  checkAll(order.laws[IVector[Int]])

  // These tests hold for Vector, so they had better hold for IVector

  implicit val intBooleanArb: Arbitrary[Int => Boolean] = {
    val intGen = implicitly[Arbitrary[Int]].arbitrary
    Arbitrary(Gen.oneOf(
      Gen.const((_: Int) => true),
      Gen.const((_: Int) => false),
      Gen.choose(2, 5).map(n => (a: Int) => a % n == 0),
      Gen.choose(2, 5).map(n => (a: Int) => a % n != 0),
      intGen.map(n => (_: Int) > n),
      intGen.map(n => (_: Int) < n)
    ))
  }

  "intercalate empty list is flatten" ! forAll { (a: IVector[IVector[Int]]) =>
    a.intercalate(IVector[Int]()) must_===(a.flatten)
  }

  "intersperse then remove odd items is identity" ! forAll { (a: IVector[Int], b: Int) =>
    val isEven = (_: Int) % 2 == 0
    a.intersperse(b).zipWithIndex.filter(p => isEven(p._2)).map(_._1) must_===(a)
  }

  "intercalate is same as a.intersperse(b).flatten" ! forAll { (a: IVector[IVector[Int]], b: IVector[Int]) =>
    a.intercalate(b) must_===(a.intersperse(b).flatten)
  }

  "intersperse vs ilist.intersperse" ! forAll { (a: IVector[Int], b: Int) =>
    IList.fromList(a.intersperse(b).toList) must_=== IList.fromList(a.toList).intersperse(b)
  }

  "foldl is foldLeft" ! forAll {(rnge: IVector[IVector[Int]]) =>
    val F = Foldable[Vector]
    rnge.foldLeft(IVector[Int]())(_++_) must_=== F.foldLeft(rnge.toVector, IVector[Int]())(_++_)
  }

  "foldr is foldRight" ! forAll {(rnge: IVector[IVector[Int]]) =>
    val F = Foldable[Vector]
    rnge.foldRight(IVector[Int]())(_++_) must_=== F.foldRight(rnge.toVector, IVector[Int]())(_++_)
  }

  "mapAccumLeft" ! forAll { xs: IVector[Int] =>
    val f = (_: Int) + 1
    xs.mapAccumLeft(IVector[Int](), (c: IVector[Int], a) => (c :+ a, f(a))) must_=== (xs, xs.map(f))
  }

  "mapAccumRight" ! forAll { xs: IVector[Int] =>
    val f = (_: Int) + 1
    xs.mapAccumRight(IVector[Int](), (c: IVector[Int], a) => (c :+ a, f(a))) must_=== (xs.reverse, xs.map(f))
  }

  // And some other tests that Vector doesn't have

  "catamorphism" ! forAll { (ns: IVector[Int]) =>
    ns.foldLeft(IVector.empty[Int]) { _ :+ _ } must_=== ns
  }

  // Functionality borrowed from Vector is tested in terms of Vector. Is this ethical?
  // Should they be collapsed into fewer cases?

  "++" ! forAll { (ns: IVector[Int], ms: IVector[Int]) =>
    (ns ++ ms).toVector must_=== ns.toVector ++ ms.toVector
  }

  "+:" ! forAll { (n: Int, ns: IVector[Int]) =>
    (n +: ns).toVector must_=== n +: ns.toVector
  }

  "/:" ! forAll { (ns: IVector[Int], s: String, f: (String, Int) => String) =>
    (s /: ns)(f) == (s /: ns.toVector)(f)
  }

  ":+" ! forAll { (n: Int, ns: IVector[Int]) =>
    (ns :+ n).toVector must_=== ns.toVector :+ n
  }

  ":\\" ! forAll { (ns: IVector[Int], s: String, f: (Int, String) => String) =>
    (ns :\ s)(f) == (ns.toVector :\ s)(f)
  }

  "collect" ! forAll { (ns: IVector[Int]) =>
    val pf: PartialFunction[Int, Int] = { case n if n % 2 == 0 => n + 1 }
    ns.collect(pf).toVector must_=== ns.toVector.collect(pf)
  }

  "collectFirst" ! forAll { (ns: IVector[Int]) =>
    val pf: PartialFunction[Int, Int] = { case n if n % 2 == 0 => n + 1 }
    ns.collectFirst(pf) must_=== ns.toVector.collectFirst(pf)
  }

  "concat" ! forAll { (ns: IVector[Int], ms: IVector[Int]) =>
    (ns concat ms).toVector must_=== ns.toVector ++ ms.toVector
  }

  "containsSlice" ! forAll { (ns: IVector[Int], ms: IVector[Int]) =>
    ns.containsSlice(ms) must_=== ns.toVector.containsSlice(ms.toVector)
  }

  "count" ! forAll { (ns: IVector[Int], p: Int => Boolean) =>
    ns.count(p) must_=== ns.toVector.count(p)
  }

  "drop" ! forAll { (ns: IVector[Int], n: Int) =>
    ns.drop(n).toVector must_=== ns.toVector.drop(n)
  }

  "dropRight" ! forAll { (ns: IVector[Int], n: Int) =>
    ns.dropRight(n).toVector must_=== ns.toVector.dropRight(n)
  }

  "dropRightWhile" ! forAll { (ns: IVector[Int], p: Int => Boolean) =>
    ns.dropRightWhile(p).toVector must_=== ns.toVector.reverse.dropWhile(p).reverse
  }

  "dropWhile" ! forAll { (ns: IVector[Int], p: Int => Boolean) =>
    ns.dropWhile(p).toVector must_=== ns.toVector.dropWhile(p)
  }

  "endsWith" ! forAll { (ns: IVector[Int], ms: IVector[Int]) =>
    ns.endsWith(ms) must_=== ns.toVector.endsWith(ms.toVector)
  }

  "fill" ! forAll { (a: Byte, b: Int) =>
    IVector.fill(a)(b).toVector must_=== Vector.fill(a)(b)
  }

  "filter" ! forAll { (ns: IVector[Int], p: Int => Boolean) =>
    ns.filter(p).toVector must_=== ns.toVector.filter(p)
  }

  "filterNot" ! forAll { (ns: IVector[Int], f: Int => Boolean) =>
    ns.filterNot(f).toVector must_=== ns.toVector.filterNot(f)
  }

  "find" ! forAll { (ns: IVector[Int], f: Int => Boolean) =>
    ns.find(f) must_=== ns.toVector.find(f)
  }

  // flatMap and folds are covered by laws

  // test get in terms of foldLeft, optionally modded into vector space
  "get" ! forAll { (ns: IVector[Int], i: Int, mod: Boolean) =>
    val index = if (mod && ns.length == 0) 0 else if (mod) i % ns.length else i

    val received = ns get index

    val expected = if (index < 0 || index >= ns.length) {
      None
    } else {
      val (_, back) = ns.foldLeft((0, None: Option[Int])) {
        case ((`index`, None), n) => (0, Some(n))
        case ((_, Some(n)), _) => (0, Some(n))
        case ((i, None), n) => (i + 1, None)
      }

      back
    }

    received must_=== expected
  }

  "groupBy" ! forAll { (ns: IVector[Int], f: Int => Int) =>
    ns.groupBy(f).map(_.toVector).toList.toMap must_=== ns.toVector.groupBy(f)
  }

  "groupBy1" ! forAll { (ns: IVector[Int], f: Int => Int) =>
    ns.groupBy1(f).map(oa => (oa.tail :+ oa.head).toVector).toList.toMap must_=== ns.toVector.groupBy(f)
  }

  "headOption" ! forAll { ns: IVector[Int] =>
    ns.headOption must_=== ns.toVector.headOption
  }

  "index" ! forAll { (ns: IVector[Int], n: Int) =>
    ns.index(n) must_=== ns.toVector.lift(n)
  }

  "indexOf" ! forAll { (ns: IVector[Int], n: Int) =>
    ns.indexOf(n).getOrElse(-1) must_=== ns.toVector.indexOf(n)
  }

  "indexOfSlice" ! forAll { (ns: IVector[Int], ms: IVector[Int]) =>
    ns.indexOfSlice(ms).getOrElse(-1) must_=== ns.toVector.indexOfSlice(ms.toVector)
  }

  "indexWhere" ! forAll { (ns: IVector[Int], f: Int => Boolean) =>
    ns.indexWhere(f).getOrElse(-1) must_=== ns.toVector.indexWhere(f)
  }

  "initOption" ! forAll { ns: IVector[Int] =>
    ns.initOption.map(_.toVector) must_=== (try Some(ns.toVector.init) catch { case e: Exception => None })
  }

  "inits" ! forAll { ns: IVector[Int] =>
    ns.inits.map(_.toVector).toVector must_=== ns.toVector.inits.toVector
  }

  // intersperse is tested above
  // isEmpty is tested by empty laws

  "lastIndexOf" ! forAll { (ns: IVector[Int], n: Int) =>
    ns.lastIndexOf(n).getOrElse(-1) must_=== ns.toVector.lastIndexOf(n)
  }

  "lastIndexOfSlice" ! forAll { (ns: IVector[Int], ms: IVector[Int]) =>
    ns.lastIndexOfSlice(ms).getOrElse(-1) must_=== ns.toVector.lastIndexOfSlice(ms.toVector)
  }

  "lastIndexWhere" ! forAll { (ns: IVector[Int], f: Int => Boolean) =>
    ns.lastIndexWhere(f).getOrElse(-1) must_=== ns.toVector.lastIndexWhere(f)
  }

  "lastOption" ! forAll { ns: IVector[Int] =>
    ns.lastOption must_=== ns.toVector.lastOption
  }

  "length" ! forAll { ns: IVector[Int] =>
    ns.length must_=== ns.toVector.length
  }

  // map is tested by functor laws

  "nonEmpty" ! forAll { ns: IVector[Int] =>
    ns.nonEmpty must_=== ns.toVector.nonEmpty
  }

  "padTo" ! forAll { (ns: IVector[Int], n: Int) =>
    ns.padTo(100, n).toVector must_=== ns.toVector.padTo(100, n)
  }

  "patch" ! forAll { (ns: IVector[Int], a: Int, ms: IVector[Int], b: Int) =>
    ns.patch(a, ms, b).toVector must_=== ns.toVector.patch(a, ms.toVector, b)
  }

  "prefixLength" ! forAll { (ns: IVector[Int], f: Int => Boolean) =>
    ns.prefixLength(f) must_=== ns.toVector.prefixLength(f)
  }

  "reduceLeftOption" ! forAll { (ns: IVector[Int], f: (Int, Int) => Int) =>
    ns.reduceLeftOption(f) must_=== ns.toVector.reduceLeftOption(f)
  }

  "reduceRightOption" ! forAll { (ns: IVector[Int], f: (Int, Int) => Int) =>
    ns.reduceRightOption(f) must_=== ns.toVector.reduceRightOption(f)
  }

  "prefixLength" ! forAll { (ns: IVector[Int], f: Int => Boolean) =>
    ns.prefixLength(f) must_=== ns.toVector.prefixLength(f)
  }

  "reverse" ! forAll { ns: IVector[Int] =>
    ns.reverse.toVector must_=== ns.toVector.reverse
  }

  "reverseMap" ! forAll { (ns: IVector[Int], f: Int => Int) =>
    ns.reverseMap(f).toVector must_=== ns.toVector.reverseMap(f)
  }

  "scanLeft" ! forAll { (ss: IVector[String], f: (Int, String) => Int) =>
    ss.scanLeft(0)(f).toVector must_=== ss.toVector.scanLeft(0)(f)
    ss.scanLeft("z")(_ + _).toVector must_=== ss.toVector.scanLeft("z")(_ + _)
    ss.scanLeft(IVector.empty[String])(_ :+ _).toVector must_=== ss.toVector.scanLeft(IVector.empty[String])(_ :+ _)
  }

  "scanRight" ! forAll { (ss: IVector[String], f: (String, Int) => Int)  =>
    ss.scanRight(0)(f).toVector must_=== ss.toVector.scanRight(0)(f)
    ss.scanRight("z")(_ + _).toVector must_=== ss.toVector.scanRight("z")(_ + _)
    ss.scanRight(IVector.empty[String])(_ +: _).toVector must_=== ss.toVector.scanRight(IVector.empty[String])(_ +: _)
  }

  "slice" ! forAll { (ns: IVector[Int], a: Int, b: Int) =>
    ns.slice(a, b).toVector must_=== ns.toVector.slice(a, b)
  }

  "sortBy" ! forAll { (ss: IVector[String], f: String => Int) =>
    ss.sortBy(f).toVector must_=== ss.toVector.sortBy(f)
  }

  "sorted" ! forAll { (ss: IVector[String]) =>
    ss.sorted.toVector must_=== ss.toVector.sorted
  }

  "span" ! forAll { (ns: IVector[Int], f: Int => Boolean) =>
    ns.span(f).umap(_.toVector) must_=== ns.toVector.span(f)
  }

  "splitAt" ! forAll { (ns: IVector[Int], n: Int) =>
    ns.splitAt(n).umap(_.toVector) must_=== ns.toVector.splitAt(n)
  }

  "startsWith" ! forAll { (ns: IVector[Int], ms: IVector[Int]) =>
    ns.startsWith(ms) must_=== ns.toVector.startsWith(ms.toVector)
  }

  "tails" ! forAll { ns: IVector[Int] =>
    ns.tails.map(_.toVector).toVector must_=== ns.toVector.tails.toVector
  }

  "tailOption" ! forAll { ns: IVector[Int] =>
    ns.tailOption.map(_.toVector) must_=== (try Some(ns.toVector.tail) catch { case e: Exception => None })
  }

  "take" ! forAll { (ns: IVector[Int], n: Byte) =>
    ns.take(n).toVector must_=== ns.toVector.take(n)
  }

  "takeRight" ! forAll { (ns: IVector[Int], n: Byte) =>
    ns.takeRight(n).toVector must_=== ns.toVector.takeRight(n)
  }

  "takeRightWhile" ! forAll { (ns: IVector[Int], f: Int => Boolean) =>
    ns.takeRightWhile(f).toVector must_=== ns.toVector.reverse.takeWhile(f).reverse
  }

  "takeWhile" ! forAll { (ns: IVector[Int], f: Int => Boolean) =>
    ns.takeWhile(f).toVector must_=== ns.toVector.takeWhile(f)
  }

  "toEphemeralStream" ! forAll { ns: Vector[Int] =>
    IVector(ns: _*).toEphemeralStream.toVector must_=== EphemeralStream(ns: _*).toVector
  }

  "toVector" ! forAll { ns: Vector[Int] =>
    IVector(ns: _*).toVector must_=== ns
  }

  "toMap" ! forAll { ps: Vector[(String, Int)] =>
    IVector(ps: _*).toMap must_=== ==>>(ps: _*)
  }

  "toNel" ! forAll { ns: Vector[Int] =>
    IVector(ns: _*).toNel must_=== Scalaz.ToVectorOpsFromVector(ns).toNel
  }

  "toStream" ! forAll { ns: Vector[Int] =>
    IVector(ns: _*).toStream must_=== ns.toStream
  }

  "toVector" ! forAll { ns: Vector[Int] =>
    IVector(ns: _*).toVector must_=== ns
  }

  "toZipper" ! forAll { ns: Vector[Int] =>
    IVector(ns: _*).toZipper must_=== scalaz.std.stream.toZipper(ns.toStream)
  }

  // uncons is tested everywhere

  // like some of the other tests, this is terrible and almost vacuous
  "updated" ! forAll { (ns: IVector[Int], i: Int, n: Int) =>
    if (i < 0 || i >= ns.length) {
      ns.updated(i, n) must_=== ns
    } else {
      ns.updated(i, n).toVector must_=== ns.toVector.updated(i, n)
    }
  }

  "unzip" ! forAll { (ns: IVector[(Int, String)]) =>
    ns.unzip.bimap(_.toVector, _.toVector) must_=== ns.toVector.unzip
  }

  // widen is tested by toMap and unzip
  // zip is tested by zip laws

  "zipWithIndex" ! forAll { ns: IVector[Int] =>
    ns.zipWithIndex.toVector must_=== ns.toVector.zipWithIndex
  }

  "any is lazy" ! FoldableTests.anyIsLazy[IVector, Int]

  "all is lazy" ! FoldableTests.allIsLazy[IVector, Int]

}
