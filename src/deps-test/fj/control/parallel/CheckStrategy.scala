package fj.control.parallel

import org.scalacheck.Prop._
import fj.ArbitraryP.arbitraryP1
import fj.control.parallel.ArbitraryStrategy.arbitraryStrategy
import fj.data.ArbitraryList.arbitraryList
import fj.data.ArbitraryArray.arbitraryArray
import Strategy.parFlatMap
import fj.P
import fj.Function.compose
import fj.data.List.{single}
import fj.data.List
import fj.data.Array
import fj.data.Array.array
import fj.Implicit._
import fj.pre.Equal.{listEqual, stringEqual, arrayEqual}

object CheckStrategy {

  def rev = (x: String) => x.reverse: String
  def id[A] = (x: A) => x: A

  val prop_par = property((a: P1[Int], s: Strategy[Int]) => a._1 == s.par(a)._1)

  val prop_parMapList = property((a: List[String], s: Strategy[String]) =>
    listEqual(stringEqual).eq(s.parMap(rev, a)._1, a.map(compose(P1.__1[String], s.concurry[String](rev)))))

  val prop_parMapArray = property((a: Array[String], s: Strategy[String]) =>
    arrayEqual(stringEqual).eq(s.parMap(rev, a)._1, a.map(compose(P1.__1[String], s.concurry[String](rev)))))

  val prop_parFlatMapList = property((a: List[String], st: Strategy[List[String]]) => {
    def f = (x: String) => single[String](x)
    listEqual(stringEqual).eq(parFlatMap(st, f, a)._1, a.bind(compose(P1.__1[List[String]], st.concurry[String](f))))
  })

  val prop_parFlatMapArray = property((a: Array[String], st: Strategy[Array[String]]) => {
    def f = (x: String) => array[String](scala.Array(x): _*)
    arrayEqual(stringEqual).eq(parFlatMap(st, f, a)._1, a.bind(compose(P1.__1[Array[String]], st.concurry[String](f))))
  })

  val prop_xmapID = property((s: Strategy[Int], n: P1[Int]) =>
    s.xmap(id[P1[Int]], id[P1[Int]]).par(n)._1 == s.par(n)._1)

  val prop_xmapCompose = property((a: Strategy[String], s: P1[String]) => {
    def f = (s: P1[String]) => P.p(s._1.toLowerCase): P1[String]
    def g = (s: P1[String]) => P.p(s._1.toUpperCase): P1[String]
    def fr = (s: P1[String]) => P.p(rev(s._1.toLowerCase)): P1[String]
    def gr = (s: P1[String]) => P.p(rev(s._1.toUpperCase)): P1[String]
    stringEqual.eq(a.xmap(f, g).xmap(fr, gr).par(s) _1, a.xmap(f.compose(fr), gr.compose(g)).par(s)._1)
  })

  val tests = scala.List(
    ("prop_par", prop_par),
    ("prop_parMapList", prop_parMapList),
    ("prop_parMapArray", prop_parMapArray),
    ("prop_parFlatMapList", prop_parFlatMapList),
    ("prop_parFlatMapArray", prop_parFlatMapArray),
    ("prop_xmapID", prop_xmapID),
    ("prop_xmapCompose", prop_xmapCompose)
    ).map{
    case (n, p) => ("Strategy." + n, p)
  }

  def main(args: scala.Array[String]) = Tests.run(tests)
}
