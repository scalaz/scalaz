package scalaz

import std.AllInstances._

object SemigroupTest extends SpecLite {
  "invariant functor" in {
    import InvariantFunctorTest._
    import syntax.invariantFunctor._

    val sg: Semigroup[Num] = Semigroup[Int].xmap[Num](Num.apply _, _.x)
    sg.append(Num(1), Num(2)) must_===(Num(3))
  }
}
