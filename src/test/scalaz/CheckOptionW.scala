package scalaz

import fjs.test.Property._
import fjs.test.Coarbitrary.coarbSInt
import fjs.test.Arbitrary.arbUSASCIIString
import fj.test.CheckResult.summaryEx
import OptionW._

object CheckOptionW {
  val prop_fold = prop((o: Option[Int], none: String, some: Int => String) => o.fold(none, some) == o.map(some).getOrElse(none))

  val prop_if = prop((o: Option[Int], x: String, y: String) => o ? (x, y) == (if(o.isEmpty) x else y))

  val prop_ifNone = prop((o: Option[Int]) => {
    var x = 0
    o ifNone(x = 1)
    o.isEmpty == (x == 1)
  })

  val prop_err = prop((o: Option[Int], s: String) => try {
    o.err(s) == o.get
  } catch {
    case e => e.getMessage == s
  })

  val prop_or = prop((o: Option[Int], x: Int) => (o | x) == o.getOrElse(x))

  val prop_toNull = prop((o: Option[Int]) => o.toNull == (o match {
    case None => null
    case Some(x) => x
  }))

  val prop_some = prop((n: Int) => some(n) == Some(n))

  val prop_somes = prop((s: List[Option[Int]]) => somes(s) == s.filter(_.isDefined).map(_.get))

  val prop_cond = prop((c: Boolean, n: Int) => cond(c, n).isEmpty || cond(c, n).map(_ == n).get)

  val props = List(prop_fold, prop_if, prop_ifNone, prop_err, prop_or, prop_toNull, prop_some, prop_somes, prop_cond)

  def run = props foreach (p => summaryEx println +p)
  def main(args: Array[String]) = run
}
