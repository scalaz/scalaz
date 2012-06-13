package scalaz

import std.AllInstances._
import std.option._
import syntax.functor._

class FunctorTest extends Spec {

  "mapply" in {
    1.mapply(some((a: Int) => a)) must be_===(some(1))
  }

  "map" in {
    (some(1) fmap (1+)) must be_===(some(2))
  }

  "strength" in {
    some(1).strengthL(2) must be_===(some((2, 1)))
    some(1).strengthR(2) must be_===(some((1, 2)))
  }

  "fpair" in {
    some(1).fpair must be_===(some((1, 1)))
  }
}
