package scalaz

import std.AllInstances._
import Tags.{Multiplication => Mult}

object TagTest2 extends SpecLite {
  "k.@@" should {
    "be abstract" in {
      val r = (List[Int](42) ++ List[Int @@ Mult]()).toSet
      r: Set[Any]
      true
    }
  }
}
