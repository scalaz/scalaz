package scalaz

import std.AllInstances._
import std.option.{some, none}
import syntax.either._

class MonadTransTest extends Spec {

  "liftM" in {
    MonadTrans[OptionT].liftM(List(1, 2, 3)).run must be_===(List(some(1), some(2), some(3)))
  }

  "liftMU" in {
    IndexedStateT.StateMonadTrans[Int].liftMU(10.right[String]).run(1) must be_===((1, 10).right[String])
  }
}
