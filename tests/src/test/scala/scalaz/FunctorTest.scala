package scalaz

import std.AllInstances._
import std.option.some
import syntax.functor._
import org.scalacheck.Prop.forAll

object FunctorTest extends SpecLite {

  "mapply" in {
    1.mapply(some((a: Int) => a)) must_===(some(1))
  }

  "map" in {
    (some(1) ∘ (1+)) must_===(some(2))
  }

  "strength" in {
    some(1).strengthL(2) must_===(some((2, 1)))
    some(1).strengthR(2) must_===(some((1, 2)))
  }

  "fpair" in {
    some(1).fpair must_===(some((1, 1)))
  }

  "widen" ! forAll { ola: Option[List[Int]] =>
    import std.iterable._
    val oia: Option[Iterable[Int]] = ola
    ola.widen[Iterable[Int]] must_===(oia)
  }
}
