package fj.data

import org.scalacheck.Prop._
import ArbitraryList.arbitraryList
import ArbitraryP.arbitraryP1
import fj.pre.Equal.{listEqual, stringEqual, p2Equal}
import fj.P.p
import fj.Unit.unit
import List.{nil, single, join, iterateWhile}
import Implicit._

object CheckList {
  val prop_isEmpty = property((a: List[Int]) =>
    a.isEmpty != a.isNotEmpty)

  val prop_isNotEmpty = property((a: List[Int]) =>
    a.length > 0 ==> a.isNotEmpty)

  val prop_orHead = property((a: List[Int], n: P1[Int]) =>
    a.isNotEmpty ==>
    (a.orHead(n) == a.head))

  val prop_orTail = property((a: List[String], n: P1[List[String]]) =>
    a.isNotEmpty ==>
    (listEqual(stringEqual).eq(a.orTail(n), a.tail)))

  val prop_toOption = property((a: List[Int]) =>
    a.toOption.isNone || a.toOption.some == a.head)

  // crashes the type checker for unknown reason
  // val prop_toEither = property((a: List[Int], n: P1[Int]) =>
  //   (a.toEither(n).isLeft && a.toEither(n).left.value == n._1) || (a.toEither(n).right.value == a.head))

  val prop_cons1 = property((a: List[Int], n: Int) =>
    a.cons(n).head == n)

  val prop_cons2 = property((a: List[Int], n: Int) =>
    a.cons(n).length == a.length + 1)

  val prop_mapId = property((a: List[String]) =>
    listEqual(stringEqual).eq(a.map((x: String) => x), a))

  val prop_mapCompose = property((a: List[String]) => {
    def f(s: String) = s.toLowerCase
    def g(s: String) = s.toUpperCase
    listEqual(stringEqual).eq(a.map((x: String) => f(g(x))), a.map((x: String) => g(x)).map((x: String) => f(x)))})

  // crashes the type checker for unknown reason
  // val prop_foreach = property((a: List[Int]) => {
  //   var i = 0
  //   a.foreach({
  //     (x: Int) => i = i + x
  //     unit
  //   })
  //
  //   var j = 0
  //
  //   val aa = a.toArray
  //
  //   for(x <- 0 until aa.length)
  //     j = j + aa.get(x)
  //
  //   i == j
  // })

  val prop_filter1 = property((a: List[Int]) =>
    a.filter((x: Int) => ((x % 2 == 0): java.lang.Boolean)).forall((x: Int) => ((x % 2 == 0): java.lang.Boolean)))

  val prop_filter2 = property((a: List[Int]) =>
    a.filter((x: Int) => ((x % 2 == 0): java.lang.Boolean)).length <= a.length)

  val prop_bindLeftIdentity = property((s: String) => {
    def f(s: String) = single[String](s.reverse)
    listEqual(stringEqual).eq(
      single[String](s).bind(f(_: String)),
      f(s))})

  val prop_bindRightIdentity = property((a: List[String]) =>
    listEqual(stringEqual).eq(
      a.bind((x: String) => single[String](x)),
      a))

  val prop_bindAssociativity = property((a: List[String]) => {
    def f(s: String) = single[String](s.reverse)
    def g(s: String) = single[String](s.toUpperCase)
    listEqual(stringEqual).eq(
      a.bind(f(_: String)).bind(g(_: String)),
      a.bind(f(_: String).bind(g(_: String))))})

  val prop_sequence = property((a: List[String], b: List[String]) =>
    listEqual(stringEqual).eq(
      a.sequence(b),
      a.bind((x: String) => b)))

  val prop_append = property((a: List[String], b: String) =>
    listEqual(stringEqual).eq(
      single(b).append(a),
      a.cons(b)))

  val prop_foldRight = property((a: List[String]) => listEqual(stringEqual).eq(
      a.foldRight((a: String, b: List[String]) => b.cons(a), nil[String]), a))

  val prop_foldLeft = property((a: List[String], s: String) =>
    listEqual(stringEqual).eq(
      a.foldLeft(((a: List[String], b: String) => single(b).append(a)), nil[String]),
      a.reverse.foldRight((a: String, b: List[String]) => single(a).append(b), nil[String])))

  val prop_length = property((a: List[String]) =>
    a.length != 0 ==>
    (a.length - 1 == a.tail.length))

  val prop_reverse = property((a: List[String], b: List[String]) =>
    listEqual(stringEqual).eq(
      (a append b).reverse,
      b.reverse.append(a.reverse)))

  val prop_index = property((a: List[String], n: Int) =>
    (n > 0 && n < a.length) ==>
    (a.index(n) == a.tail.index(n - 1)))

  val prop_snoc = property((a: List[String], s: String) =>
    listEqual(stringEqual).eq(
      a.snoc(s),
      a.append(single(s))))

  val prop_take = property((a: List[String], n: Int) =>
    a.take(n).length() <= a.length())

  val prop_drop = property((a: List[String], n: Int) =>
    a.drop(n).length() <= a.length())

  val prop_splitAt = property((a: List[String], n: Int) =>
    p2Equal(listEqual(stringEqual), listEqual(stringEqual)).eq(
      a.splitAt(n),
      p(a.take(n), a.drop(n))))

  val prop_partition = property((a: List[String], n: Int) => (a.length > 0 && n > 0) ==>
    {
      val as = a.partition(n)
      listEqual(stringEqual).eq(join(as), a) && as.forall((x: List[String]) => (x.length <= n):java.lang.Boolean)
    })

  val prop_inits = property((a: List[String]) =>
    a.isEmpty || a.inits.length == a.length + 1 && join(a.inits).length == 1.to(a.length).foldLeft(0)(_+_))

  val prop_tails = property((a: List[String]) =>
    a.isEmpty || a.tails.length == a.length + 1 && join(a.tails).length == 1.to(a.length).foldLeft(0)(_+_))

  val prop_sort = property((a: List[String]) =>
    {
      val s = a.sort(fj.pre.Ord.stringOrd)
      s.isEmpty || s.tail.isEmpty || s.head.compareTo(s.tail.head) <= 0
    })

  val prop_forall = property((a: List[Int]) =>
    a.forall((x: Int) => ((x % 2 == 0): java.lang.Boolean)) ==
    !a.exists((x: Int) => ((x % 2 != 0): java.lang.Boolean)))

  val prop_exists = property((a: List[Int]) =>
    a.exists((x: Int) => ((x % 2 == 0): java.lang.Boolean)) ==
    !a.forall((x: Int) => ((x % 2 != 0): java.lang.Boolean)))

  val prop_find = property((a: List[Int]) => {
    val s = a.find((x: Int) => (x % 2 == 0): java.lang.Boolean)
    s.forall((x: Int) => (x % 2 == 0): java.lang.Boolean)
  })

  val prop_nub = property((a: List[String], b: List[String]) =>
    listEqual(stringEqual).eq(a append b nub, a.nub.append(b.nub).nub))

  val prop_join = property((a: List[List[String]]) =>
    listEqual(stringEqual).eq(
      a.foldRight((a: List[String], b: List[String]) => a.append(b), nil[String]),
      join(a)))

  val prop_iterateWhile = property((n: Int) => n > 0 ==>
    (iterateWhile(((x:Int) => x - 1), ((x:Int) => ((x > 0): java.lang.Boolean)), n).length == n))

  val tests = scala.List(
      ("prop_isEmpty", prop_isEmpty),
      ("prop_isNotEmpty", prop_isNotEmpty),
      ("prop_orHead", prop_orHead),
      ("prop_orTail", prop_orTail),
      ("prop_toOption", prop_toOption),
//      ("prop_toEither", prop_toEither),
      ("prop_cons1", prop_cons1),
      ("prop_cons2", prop_cons2),
      ("prop_mapId", prop_mapId),
      ("prop_mapCompose", prop_mapCompose),
//      ("prop_foreach", prop_foreach),
      ("prop_filter1", prop_filter1),
      ("prop_filter2", prop_filter2),
      ("prop_bindLeftIdentity", prop_bindLeftIdentity),
      ("prop_bindRightIdentity", prop_bindRightIdentity),
      ("prop_bindAssociativity", prop_bindAssociativity),
      ("prop_sequence", prop_sequence),
      ("prop_append", prop_append),
      ("prop_foldRight", prop_foldRight),
      ("prop_foldLeft", prop_foldLeft),
      ("prop_length", prop_length),
      ("prop_reverse", prop_reverse),
      ("prop_index", prop_index),
      ("prop_snoc", prop_snoc),
      ("prop_take", prop_take),
      ("prop_drop", prop_drop),
      ("prop_splitAt", prop_splitAt),
      ("prop_partition", prop_partition),
      ("prop_inits", prop_inits),
      ("prop_tails", prop_tails),
      ("prop_sort", prop_sort),
      ("prop_forall", prop_forall),
      ("prop_exists", prop_exists),
      ("prop_find", prop_find),
      ("prop_nub", prop_nub),
      ("prop_iterateWhile", prop_iterateWhile)
  ).map { case (n, p) => ("List." + n, p) }

  def main(args: scala.Array[String]) = Tests.run(tests)  
}
