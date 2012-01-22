package scalaz

import std.AllInstances._
import syntax.apply._
import std.option._

class ApplyTest extends Spec {
  "mapN" in {
    Apply[Option].map2(some(1), some(2))(_ + _) must be_===(some(3))
    Apply[Option].map3(some(1), some(2), some(3))(_ + _ + _) must be_===(some(6))
    Apply[Option].map4(some(1), some(2), some(3), some(4))(_ + _ + _ + _) must be_===(some(10))
    Apply[Option].map5(some(1), some(2), some(3), some(4), some(5))(_ + _ + _ + _ + _) must be_===(some(15))
  }
}