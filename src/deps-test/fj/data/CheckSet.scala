package fj.data

import org.scalacheck.Prop._
import fj.data.ArbitrarySet.arbitrarySet
import fj.data.ArbitraryList.arbitraryList
import fj.ArbitraryP.arbitraryP1
import fj.pre.Equal.{setEqual, stringEqual, listEqual}
import fj.pre.Ord.intOrd
import fj.pre.Ord.stringOrd
import fj.pre.Ord
import fj.P.p
import fj.Unit.unit
import Set.{empty, single, join, iterableSet}
import fj.Implicit._

object CheckSet {

  def idInt(n: Int) = n:java.lang.Integer
  implicit def oi : Ord[Int] = intOrd.comap(idInt _)
  implicit def os : Ord[String] = stringOrd

  val prop_isEmpty = property((a: Set[Int]) =>
    a.isEmpty == (a.size == 0))

  val prop_isNotEmpty = property((a: Set[Int]) =>
    !a.isEmpty == (a.size > 0))

  val prop_insertMember = property((a: Set[Int], n: Int) =>
    a.insert(n).member(n))

  val prop_deleteInsertIsId = property((a: Set[String], s: String) =>
    setEqual(stringEqual).eq(a.delete(s).insert(s).delete(s), a.delete(s)))

  val prop_deleteSize = property((a: Set[String], s: String) =>
    (a.insert(s).size == a.size + 1) != a.member(s))

  val prop_singleMember = property((n: Int) =>
    single(oi, n).member(n))

  val prop_noDupesFromList = property((a: List[String]) =>
    setEqual(stringEqual).eq(iterableSet(os, a.nub(stringEqual)), iterableSet(os, a)))

  val prop_noDupesToList = property((a: List[String]) =>
    iterableSet(os, a).toList().length() == a.nub(stringEqual).length())

  val prop_subsetEmpty = property((a: Set[Int]) =>
    empty(oi).subsetOf(a))

  val prop_subsetUnion = property((a: Set[Int], b: Set[Int]) =>
    b.subsetOf(a.union(b)))

  val prop_subsetSelf = property((a: Set[Int]) =>
    a.subsetOf(a))

  val prop_subsetSize = property((a: Set[Int], b: Set[Int]) =>
    a.size > b.size ==> !a.subsetOf(b))

  val prop_mapId = property((a: Set[String]) =>
    setEqual(stringEqual).eq(a.map(os, (x: String) => x), a))

    val tests = scala.List(
      ("prop_isEmpty", prop_isEmpty),
      ("prop_isNotEmpty", prop_isNotEmpty),
      ("prop_insertMember", prop_insertMember),
      ("prop_deleteInsertIsId", prop_deleteInsertIsId),
      ("prop_deleteSize", prop_deleteSize),
      ("prop_singleMember", prop_singleMember),
      ("prop_noDupesFromList", prop_noDupesFromList),
      ("prop_noDupesToList", prop_noDupesToList),
      ("prop_subsetEmpty", prop_subsetEmpty),
      ("prop_subsetUnion", prop_subsetUnion),
      ("prop_subsetSelf", prop_subsetSelf),
      ("prop_subsetSize", prop_subsetSize),
      ("prop_mapId", prop_mapId)
  ).map { case (n, p) => ("Set." + n, p) }

  def main(args: scala.Array[String]) = Tests.run(tests)
}