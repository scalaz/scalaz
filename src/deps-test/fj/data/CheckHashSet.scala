package fj.data

import org.scalacheck.Prop._
import ArbitraryHashSet._
import pre.Equal.intEqual
import pre.Hash.intHash
import fjs.F._

object CheckHashSet {
  implicit val equalInt: pre.Equal[Int] = intEqual comap ((x: Int) => (x: java.lang.Integer))
  implicit val hashInt: pre.Hash[Int] = intHash comap ((x: Int) => (x: java.lang.Integer))

  val prop_eq = property((s: HashSet[Int], x: Int, y: Int) => s.eq(x, y) == equalInt.eq(x, y))

  val prop_hash = property((s: HashSet[Int], x: Int) => s.hash(x) == hashInt.hash(x))

  val prop_set = property((s: HashSet[Int], x: Int) => {
    s.set(x)
    s.contains(x)
  })

  val prop_clear = property((s: HashSet[Int], k: Int) => {
    s.clear
    !s.contains(k)
  })

  val prop_isEmpty = property((s: HashSet[Int], k: Int) => !s.contains(k) || !s.isEmpty)

  val prop_size = property((s: HashSet[Int], k: Int) => !s.contains(k) || s.size != 0)

  val prop_delete = property((s: HashSet[Int], k: Int) => {
    s.delete(k)
    !s.contains(k)
  })

  val tests = scala.List(
      ("prop_eq", prop_eq),
      ("prop_hash", prop_hash),
      ("prop_set", prop_set),
      ("prop_isEmpty", prop_isEmpty),
      ("prop_size", prop_size),
      ("prop_delete", prop_delete)
  ).map { case (n, p) => ("HashSet." + n, p) }

  def main(args: scala.Array[String]) = Tests.run(tests)

}
