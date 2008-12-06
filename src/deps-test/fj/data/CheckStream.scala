package fj.data

import org.scalacheck.Prop._
import ArbitraryStream.arbitraryStream
import ArbitraryP.arbitraryP1
import fj.pre.Equal.{streamEqual, stringEqual}
import Implicit._
import fj.Unit.unit
import Stream.{nil, single, join}

object CheckStream {
  val prop_isEmpty = property((a: Stream[Int]) =>
    a.isEmpty != a.isNotEmpty)

  val prop_isNotEmpty = property((a: Stream[Int]) =>
    a.length > 0 ==> a.isNotEmpty)

  val prop_orHead = property((a: Stream[Int], n: P1[Int]) =>
    a.isNotEmpty ==>
    (a.orHead(n) == a.head))

  val prop_orTail = property((a: Stream[String], n: P1[Stream[String]]) =>
    a.isNotEmpty ==>
    (streamEqual(stringEqual).eq(a.orTail(n)._1, a.tail._1)))

  val prop_toOption = property((a: Stream[Int]) =>
    a.toOption.isNone || a.toOption.some == a.head)

  // crashes the type checker for unknown reason
  // val prop_toEither = property((a: Stream[Int], n: P1[Int]) =>
  //   (a.toEither(n).isLeft && a.toEither(n).left.value == n._1) || (a.toEither(n).right.value == a.head))

  val prop_cons1 = property((a: Stream[Int], n: Int) =>
    a.cons(n).head == n)

  val prop_cons2 = property((a: Stream[Int], n: Int) =>
    a.cons(n).length == a.length + 1)

  val prop_mapId = property((a: Stream[String]) =>
    streamEqual(stringEqual).eq(a.map((x: String) => x), a))

  val prop_mapCompose = property((a: Stream[String]) => {
    def f(s: String) = s.toLowerCase
    def g(s: String) = s.toUpperCase
    streamEqual(stringEqual).eq(a.map((x: String) => f(g(x))), a.map((x: String) => g(x)).map((x: String) => f(x)))})

  // crashes the type checker for unknown reason
  // val prop_foreach = property((a: Stream[Int]) => {
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

  val prop_filter1 = property((a: Stream[Int]) =>
    a.filter((x: Int) => ((x % 2 == 0): java.lang.Boolean)).forall((x: Int) => ((x % 2 == 0): java.lang.Boolean)))

  val prop_filter2 = property((a: Stream[Int]) =>
    a.filter((x: Int) => ((x % 2 == 0): java.lang.Boolean)).length <= a.length)

  val prop_bindLeftIdentity = property((a: Stream[String], s: String) => {
    def f(s: String) = single[String](s.reverse)
    streamEqual(stringEqual).eq(
      single[String](s).bind(f(_: String)),
      f(s))})

  val prop_bindRightIdentity = property((a: Stream[String]) =>
    streamEqual(stringEqual).eq(
      a.bind((x: String) => single[String](x)),
      a))

  val prop_bindAssociativity = property((a: Stream[String]) => {
    def f(s: String) = single[String](s.reverse)
    def g(s: String) = single[String](s.toUpperCase)
    streamEqual(stringEqual).eq(
      a.bind(f(_: String)).bind(g(_: String)),
      a.bind(f(_: String).bind(g(_: String))))})

  val prop_sequence = property((a: Stream[String], b: Stream[String]) =>
    streamEqual(stringEqual).eq(
      a.sequence(b),
      a.bind((x: String) => b)))

  val prop_append = property((a: Stream[String], b: String) =>
    streamEqual(stringEqual).eq(
      single(b).append(a),
      a.cons(b)))

  val prop_foldRight = property((a: Stream[String]) => streamEqual(stringEqual).eq(
      a.foldRight((a: String, b: P1[Stream[String]]) => b._1.cons(a), nil[String]), a))
                                     
  val prop_foldLeft = property((a: Stream[String], s: String) =>
    streamEqual(stringEqual).eq(
      a.foldLeft(((a: Stream[String], b: String) => single(b).append(a)), nil[String]),
      a.reverse.foldRight((a: String, b: P1[Stream[String]]) => single(a).append(b._1), nil[String])))

  val prop_length = property((a: Stream[String]) =>
    a.length != 0 ==>
    (a.length - 1 == a.tail._1.length))

  val prop_reverse = property((a: Stream[String], b: Stream[String]) =>
    streamEqual(stringEqual).eq(
      (a append b).reverse,
      b.reverse.append(a.reverse)))

  val prop_index = property((a: Stream[String], n: Int) =>
    (n > 0 && n < a.length) ==>
    (a.index(n) == a.tail._1.index(n - 1)))

  val prop_forall = property((a: Stream[Int]) =>
    a.forall((x: Int) => ((x % 2 == 0): java.lang.Boolean)) ==
    !a.exists((x: Int) => ((x % 2 != 0): java.lang.Boolean)))

  val prop_exists = property((a: Stream[Int]) =>
    a.exists((x: Int) => ((x % 2 == 0): java.lang.Boolean)) ==
    !a.forall((x: Int) => ((x % 2 != 0): java.lang.Boolean)))

  val prop_find = property((a: Stream[Int]) => {
    val s = a.find((x: Int) => (x % 2 == 0): java.lang.Boolean)
    s.forall((x: Int) => (x % 2 == 0): java.lang.Boolean)
  })

  val prop_join = property((a: Stream[Stream[String]]) =>
    streamEqual(stringEqual).eq(
      a.foldRight((a: Stream[String], b: P1[Stream[String]]) => a.append(b._1), nil[String]),
      join(a)))

  val tests = scala.List(
      ("prop_isEmpty", prop_isEmpty),
      ("prop_isNotEmpty", prop_isNotEmpty),
      ("prop_orHead", prop_orHead),
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
      ("prop_forall", prop_forall),
      ("prop_exists", prop_exists),
      ("prop_find", prop_find),
      ("prop_join", prop_join)
  ).map { case (n, p) => ("Stream." + n, p) }

  def main(args: scala.Array[String]) = Tests.run(tests)
}
