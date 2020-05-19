package scalaz

import std.AllInstances._

object EitherTTest2 extends SpecLite {

  "rightU" should {
    val a: String \/ Int = \/-(1)
    val b: EitherT[Boolean, ({type l[a] = String \/ a})#l, Int] = EitherT.rightU[Boolean](a)
    b must_== EitherT.rightT[Boolean, ({type l[a] = String \/ a})#l, Int](a)
  }

  def compilationTests() = {
    //compilation test for eitherTU
    {
      val se: State[Vector[String], Int \/ Float] = null
      EitherT.eitherTU(se)
      val ee: String \/ (Int \/ Float) = null
      EitherT.eitherTU(ee)
    }
  }
}
