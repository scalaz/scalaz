package scalaz.example

import scalaz.UnionTypes._

object UnionUsage {
  
  type MyUnion = t[Float]#t[Int]#t[String]

  implicitly[Contains[Float, MyUnion]]
  implicitly[Float ∈ MyUnion]

  // also...
  implicitly[String ∈ MyUnion]

  // but not...
  //implicitly[Double ∈ MyUnion]

}

// vim: expandtab:ts=2:sw=2

