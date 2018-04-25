package scalaz

import org.scalacheck.Prop.forAll
import scalaz.std.AllInstances._

object FoldableTest2 extends SpecLite {
  "to" ! forAll {
    (xs: List[Int]) =>
      val v: Vector[Int] = Foldable[List].to[Int, Vector](xs)
      v.toList must_== xs
  }
}
