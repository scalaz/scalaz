package scalaz

import std.AllInstances._
import std.option._
import syntax.functor._

class MonadPlusTest extends Spec {

  "unite" in {
    MonadPlus[List].unite(List(some(1), none[Int], some(2))) must be_===(List(1, 2))
  }

  "filter" in {
    MonadPlus[List].filter(List(1, 2, 3))(_ % 2 == 0) must be_===(List(2))
  }
}
