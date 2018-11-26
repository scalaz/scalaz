package scalaz

import scalaz.Scalaz._
import scalaz.Strong.uncurry
import org.scalacheck.Prop.forAll

object StrongSpec extends SpecLite {

  "first" ! forAll {
    (f: String => Int, pair: (String, Int)) =>
      (f.first(pair)) must_=== ((f(pair._1), pair._2))
  }

  "uncurry" ! forAll {
    (f: String => Int => String, pair: (String, Int)) =>
      val uncurried: ((String, Int)) => String = uncurry(f)
      uncurried(pair) must_=== (f(pair._1)(pair._2))
  }
}
