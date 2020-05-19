package scalaz

import std.AllInstances._
import std.option.some
import syntax.either._

object MonadTransTest extends SpecLite {

  "liftM" in {
    MonadTrans[OptionT].liftM(List(1, 2, 3)).run must_===(List(some(1), some(2), some(3)))
    val mt = IndexedStateT.StateMonadTrans[Int]
    mt.liftM(10.right[String]).run(1) must_===((1, 10).right[String])
  }
}
