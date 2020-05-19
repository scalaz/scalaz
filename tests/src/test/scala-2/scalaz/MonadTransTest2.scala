package scalaz

import std.AllInstances._
import syntax.either._

object MonadTransTest2 extends SpecLite {
  "liftMU" in {
    val mt = IndexedStateT.StateMonadTrans[Int]
    mt.liftMU(10.right[String]).run(1) must_===((1, 10).right[String])
  }
}
