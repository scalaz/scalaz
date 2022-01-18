package scalaz

import scalaz.std.AllInstances._

object BitraverseTest2 extends SpecLite {

  "bitraverseU" in {
    import syntax.bitraverse._
    val a: Validation[Int \/ String, Int \/ Boolean] = Success(\/-(true))
    val b = a.bitraverseU(identity, identity)
    val _ = b: (Int \/ Validation[String, Boolean])
    b must_=== \/-(Success(true))
  }

  "bisequenceU" in {
    import syntax.bitraverse._
    val a: Validation[Int \/ String, Int \/ Boolean] = Success(\/-(true))
    val b = a.bisequenceU
    val _ = b: (Int \/ Validation[String, Boolean])
    b must_=== \/-(Success(true))
  }

}
