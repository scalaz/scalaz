package scalaz

import std.AllInstances._
import std.option.some
import syntax.either._

object MonadTransTest extends SpecLite {

  "liftM" in {
    MonadTrans[OptionT].liftM(List(1, 2, 3)).run must_===(List(some(1), some(2), some(3)))
  }

  "liftMU" in {
    val mt = IndexedStateT.StateMonadTrans[Int]
    // Inference of M fails when using liftM with a higher kinded type
    mt.liftM[String \/ ?, Int](10.right[String]).run(1) must_===((1, 10).right[String])
    mt.liftMU(10.right[String]).run(1) must_===((1, 10).right[String])
  }
}
