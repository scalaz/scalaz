package scalaz

import std.AllInstances._
import Tags.{Multiplication => Mult}

object TagTest2 extends SpecLite {
  "of.onCov" should {
    "choose covariant position" in {
      Tag.of[Mult].onCov((a:Int) => a): (Int => (Int @@ Mult))
      true
    }
  }

  "of.onContra" should {
    "choose contravariant position" in {
      Tag.of[Mult].onContra(Show[Int]): Show[Int @@ Mult]
      true
    }
  }
}
